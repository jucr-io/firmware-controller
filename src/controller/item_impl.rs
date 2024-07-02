use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    parse_quote,
    spanned::Spanned,
    Attribute, Ident, ImplItem, ImplItemFn, ItemImpl, Result, Signature, Token, Visibility,
};

use crate::util::snake_to_pascal_case;

pub(crate) fn expand(mut input: ItemImpl) -> Result<TokenStream> {
    let struct_name = get_struct_name(&input)?;
    let struct_name_str = struct_name.to_string();
    let methods = get_methods(&mut input, &struct_name)?;

    let signal_declarations = methods.iter().filter_map(|m| match m {
        Method::Signal(signal) => Some(&signal.declarations),
        _ => None,
    });

    let methods = methods.iter().filter_map(|m| match m {
        Method::Proxied(method) => Some(method),
        _ => None,
    });
    let args_channel_declarations = methods.clone().map(|m| &m.args_channel_declarations);
    let args_channels_rx_tx = methods.clone().map(|m| &m.args_channels_rx_tx);
    let select_arms = methods.clone().map(|m| &m.select_arm);

    let run_method = quote! {
        pub async fn run(mut self) {
            #(#args_channels_rx_tx)*

            loop {
                futures::select_biased! {
                    #(#select_arms),*
                }
            }
        }
    };
    input.items.push(syn::parse2(run_method)?);

    let client_name = Ident::new(&format!("{}Client", struct_name_str), input.span());
    let client_methods = methods.clone().map(|m| &m.client_method);
    let client_method_tx_rx_declarations =
        methods.clone().map(|m| &m.client_method_tx_rx_declarations);
    let client_method_tx_rx_initializations = methods
        .clone()
        .map(|m| &m.client_method_tx_rx_initializations);

    Ok(quote! {
        #(#args_channel_declarations)*

        #input

        pub struct #client_name {
            #(#client_method_tx_rx_declarations)*
        }

        impl #client_name {
            pub fn new() -> Self {
                Self {
                    #(#client_method_tx_rx_initializations)*
                }
            }

            #(#client_methods)*
        }

        #(#signal_declarations)*
    })
}

fn get_methods(input: &mut ItemImpl, struct_name: &Ident) -> Result<Vec<Method>> {
    input
        .items
        .iter_mut()
        .filter_map(|item| match item {
            syn::ImplItem::Fn(m) => Some(ProxiedMethod::parse(m, struct_name).map(Method::Proxied)),
            syn::ImplItem::Verbatim(tokens) => {
                // … thus parse them ourselves and construct an ImplItemFn from that
                let ImplItemSignal { attrs, vis, sig } =
                    match syn::parse2::<ImplItemSignal>(tokens.clone()) {
                        Ok(decl) => decl,
                        Err(e) => return Some(Err(e)),
                    };
                *item = ImplItem::Fn(ImplItemFn {
                    attrs,
                    vis,
                    defaultness: None,
                    sig,
                    // This empty block will be replaced below.
                    block: parse_quote!({}),
                });
                match item {
                    ImplItem::Fn(m) => Some(Signal::parse(m, struct_name).map(Method::Signal)),
                    _ => unreachable!(),
                }
            }
            _ => None,
        })
        .collect::<Result<Vec<_>>>()
}

fn get_struct_name(input: &ItemImpl) -> Result<Ident> {
    match input.self_ty.as_ref() {
        syn::Type::Path(path) => {
            let path = &path.path;
            if path.segments.len() != 1 {
                return Err(syn::Error::new(
                    path.span(),
                    "Expected single segment in type path",
                ));
            }
            let segment = path.segments.first().unwrap();

            Ok(segment.ident.clone())
        }
        _ => Err(syn::Error::new(
            input.self_ty.span(),
            "Expected type path in impl block",
        )),
    }
}

#[derive(Debug)]
enum Method {
    /// A method that will be proxied.
    Proxied(ProxiedMethod),
    /// A signal method.
    Signal(Signal),
}

/// Method that will be called by the client.
// TODO: Better name.
#[derive(Debug)]
struct ProxiedMethod {
    /// The arguments' channel declarations (both for input and output).
    args_channel_declarations: TokenStream,
    /// The input arguments' receiver & output sender declarations.
    args_channels_rx_tx: TokenStream,
    /// The select! arm for proxying the method call from the input channel & method return to the
    /// output channel.
    select_arm: TokenStream,
    /// The client-side method.
    client_method: TokenStream,
    /// The client-side method's input and output channel tx & rx declarations.
    client_method_tx_rx_declarations: TokenStream,
    /// The client-side method's input and output channel tx & rx initializations.
    client_method_tx_rx_initializations: TokenStream,
}

impl ProxiedMethod {
    fn parse(method: &ImplItemFn, struct_name: &Ident) -> Result<Self> {
        let method_args = ProxiedMethodArgs::parse(method)?;

        let (args_channel_declarations, input_channel_name, output_channel_name) =
            method_args.generate_args_channel_declarations(struct_name);
        let (args_channels_rx_tx, select_arm) =
            method_args.generate_args_channel_rx_tx(&input_channel_name, &output_channel_name);
        let (client_method, client_method_tx_rx_declarations, client_method_tx_rx_initializations) =
            method_args.generate_client_method(&input_channel_name, &output_channel_name);

        Ok(Self {
            args_channel_declarations,
            args_channels_rx_tx,
            select_arm,
            client_method,
            client_method_tx_rx_declarations,
            client_method_tx_rx_initializations,
        })
    }
}

#[derive(Debug)]
struct ProxiedMethodArgs<'a> {
    method: &'a ImplItemFn,
    in_args: MethodInputArgs,
    out_type: TokenStream,
}

impl ProxiedMethodArgs<'_> {
    fn parse(method: &ImplItemFn) -> Result<ProxiedMethodArgs<'_>> {
        let in_args = MethodInputArgs::parse(method)?;
        let out_type = match &method.sig.output {
            syn::ReturnType::Type(_, ty) => quote! { #ty },
            syn::ReturnType::Default => {
                return Err(syn::Error::new(
                    method.sig.ident.span(),
                    "Expected return type in method signature",
                ))
            }
        };

        Ok(ProxiedMethodArgs {
            method,
            in_args,
            out_type,
        })
    }

    fn generate_args_channel_declarations(
        &self,
        struct_name: &Ident,
    ) -> (TokenStream, Ident, Ident) {
        let in_types = &self.in_args.types;
        let out_type = &self.out_type;
        let method_name = &self.method.sig.ident;
        let method_name_str = method_name.to_string();

        let struct_name_caps = struct_name.to_string().to_uppercase();
        let method_name_caps = method_name_str.to_uppercase();
        let input_channel_name = Ident::new(
            &format!("{struct_name_caps}_{method_name_caps}_INPUT_CHANNEL"),
            self.method.span(),
        );
        let output_channel_name = Ident::new(
            &format!("{struct_name_caps}_{method_name_caps}_OUTPUT_CHANNEL"),
            self.method.span(),
        );
        let capacity = super::ALL_CHANNEL_CAPACITY;
        let args_channel_declarations = quote! {
            static #input_channel_name:
                embassy_sync::channel::Channel<
                    embassy_sync::blocking_mutex::raw::CriticalSectionRawMutex,
                    (#(#in_types),*),
                    #capacity,
                > = embassy_sync::channel::Channel::new();
            static #output_channel_name:
                embassy_sync::channel::Channel<
                    embassy_sync::blocking_mutex::raw::CriticalSectionRawMutex,
                    #out_type,
                    #capacity,
                > = embassy_sync::channel::Channel::new();
        };

        (
            args_channel_declarations,
            input_channel_name,
            output_channel_name,
        )
    }

    // Also generates the select! arm for the method dispatch.
    fn generate_args_channel_rx_tx(
        &self,
        input_channel_name: &Ident,
        output_channel_name: &Ident,
    ) -> (TokenStream, TokenStream) {
        let in_names = &self.in_args.names;
        let method_name = &self.method.sig.ident;
        let method_name_str = method_name.to_string();
        let input_channel_rx_name =
            Ident::new(&format!("{method_name_str}_rx"), self.method.span());
        let output_channel_tx_name =
            Ident::new(&format!("{method_name_str}_tx"), self.method.span());
        let args_channels_rx_tx = quote! {
            let #input_channel_rx_name = embassy_sync::channel::Channel::receiver(&#input_channel_name);
            let #output_channel_tx_name = embassy_sync::channel::Channel::sender(&#output_channel_name);
        };
        let select_arm = quote! {
            (#(#in_names),*) = futures::FutureExt::fuse(
                embassy_sync::channel::Receiver::receive(&#input_channel_rx_name),
            ) => {
                let ret = self.#method_name(#(#in_names),*).await;

                embassy_sync::channel::Sender::send(&#output_channel_tx_name, ret).await;
            }
        };

        (args_channels_rx_tx, select_arm)
    }

    // Also generate the input and output channel declarations, and initializations.
    fn generate_client_method(
        &self,
        input_channel_name: &Ident,
        output_channel_name: &Ident,
    ) -> (TokenStream, TokenStream, TokenStream) {
        let method_name = &self.method.sig.ident;
        let in_names = &self.in_args.names;
        let in_names = if in_names.is_empty() {
            quote! { () }
        } else {
            quote! { (#(#in_names),*) }
        };
        let method_name_str = method_name.to_string();
        let input_channel_tx_name =
            Ident::new(&format!("{method_name_str}_tx"), self.method.span());
        let output_channel_rx_name =
            Ident::new(&format!("{method_name_str}_rx"), self.method.span());
        let mut method = self.method.clone();

        method.block = parse_quote!({
            // Method call.
            embassy_sync::channel::Sender::send(&self.#input_channel_tx_name, #in_names).await;

            // Method return.
            embassy_sync::channel::Receiver::receive(&self.#output_channel_rx_name).await
        });

        let in_types = &self.in_args.types;
        let out_type = &self.out_type;
        let capacity = super::ALL_CHANNEL_CAPACITY;
        let tx_rx_declarations = quote! {
            #input_channel_tx_name: embassy_sync::channel::Sender<
                'static,
                embassy_sync::blocking_mutex::raw::CriticalSectionRawMutex,
                (#(#in_types),*),
                #capacity,
            >,
            #output_channel_rx_name: embassy_sync::channel::Receiver<
                'static,
                embassy_sync::blocking_mutex::raw::CriticalSectionRawMutex,
                #out_type,
                #capacity,
            >,
        };

        let tx_rx_initializations = quote! {
            #input_channel_tx_name: embassy_sync::channel::Channel::sender(&#input_channel_name),
            #output_channel_rx_name: embassy_sync::channel::Channel::receiver(&#output_channel_name),
        };

        (
            quote! { #method },
            tx_rx_declarations,
            tx_rx_initializations,
        )
    }
}

#[derive(Debug)]
struct Signal {
    /// The input arguments' channel and client-side struct declarations.
    declarations: TokenStream,
}

impl Signal {
    fn parse(method: &mut ImplItemFn, struct_name: &Ident) -> Result<Self> {
        remove_signal_attr(method)?;

        let MethodInputArgs { types, names } = MethodInputArgs::parse(method)?;

        let method_name = &method.sig.ident;
        let method_name_str = method_name.to_string();
        let struct_name_caps = struct_name.to_string().to_uppercase();
        let method_name_caps = method_name_str.to_uppercase();
        let method_name_pascal = snake_to_pascal_case(&method_name_str);
        let args_channel_name = Ident::new(
            &format!("{struct_name_caps}_{method_name_caps}_CHANNEL"),
            method.span(),
        );
        let args_publisher_name = Ident::new(
            &format!("{struct_name_caps}_{method_name_caps}_PUBLISHER"),
            method.span(),
        );
        let subscriber_struct_name = Ident::new(
            &format!("{struct_name}{method_name_pascal}"),
            method.span(),
        );
        let args_struct_name = Ident::new(
            &format!("{struct_name}{method_name_pascal}Args"),
            method.span(),
        );

        let capacity = super::ALL_CHANNEL_CAPACITY;
        let max_subscribers = super::BROADCAST_MAX_SUBSCRIBERS;
        let max_publishers = super::BROADCAST_MAX_PUBLISHERS;

        let declarations = quote! {
            static #args_channel_name:
                embassy_sync::pubsub::PubSubChannel<
                    embassy_sync::blocking_mutex::raw::CriticalSectionRawMutex,
                    #args_struct_name,
                    #capacity,
                    #max_subscribers,
                    #max_publishers,
                > = embassy_sync::pubsub::PubSubChannel::new();

            lazy_static::lazy_static! {
                static ref #args_publisher_name: embassy_sync::pubsub::publisher::Publisher<
                    'static,
                    embassy_sync::blocking_mutex::raw::CriticalSectionRawMutex,
                    #args_struct_name,
                    #capacity,
                    #max_subscribers,
                    #max_publishers,
                    // Safety: The publisher is only initialized once.
                > = embassy_sync::pubsub::PubSubChannel::publisher(&#args_channel_name).unwrap();
            }

            #[derive(Debug, Clone)]
            pub struct #args_struct_name {
                #(pub #names: #types),*
            }

            pub struct #subscriber_struct_name {
                subscriber: embassy_sync::pubsub::subscriber::Subscriber<
                    'static,
                    embassy_sync::blocking_mutex::raw::CriticalSectionRawMutex,
                    #args_struct_name,
                    #capacity,
                    #max_subscribers,
                    #max_publishers,
                >,
            }

            impl #subscriber_struct_name {
                pub fn new() -> Option<Self> {
                    embassy_sync::pubsub::PubSubChannel::subscriber(&#args_channel_name)
                        .ok()
                        .map(|subscriber| Self { subscriber })
                }
            }

            impl futures::Stream for #subscriber_struct_name {
                type Item = #args_struct_name;

                fn poll_next(
                    self: core::pin::Pin<&mut Self>,
                    cx: &mut core::task::Context<'_>,
                ) -> core::task::Poll<Option<Self::Item>> {
                    let subscriber = core::pin::Pin::new(&mut *self.get_mut().subscriber);
                    futures::Stream::poll_next(subscriber, cx)
                }
            }
        };

        method.block = parse_quote!({
            embassy_sync::pubsub::publisher::Pub::publish(
                &#args_publisher_name,
                #args_struct_name { #(#names),* },
            ).await;
        });

        Ok(Self { declarations })
    }
}

// Like ImplItemFn, but with a semicolon at the end instead of a body block
struct ImplItemSignal {
    attrs: Vec<Attribute>,
    vis: Visibility,
    sig: Signature,
}

impl Parse for ImplItemSignal {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let vis = input.parse()?;
        let sig = input.parse()?;
        let _: Token![;] = input.parse()?;

        Ok(ImplItemSignal { attrs, vis, sig })
    }
}

fn remove_signal_attr(method: &mut ImplItemFn) -> syn::Result<()> {
    method.attrs = method
        .attrs
        .iter()
        .cloned()
        .filter_map(|attr| {
            if !attr.path().is_ident("controller") {
                return Some(Ok(attr));
            }

            let res = attr.parse_nested_meta(|meta| {
                if !meta.path.is_ident("signal") {
                    let e = format!(
                        "expected `signal` attribute, found `{}`",
                        meta.path.get_ident().unwrap()
                    );

                    return Err(syn::Error::new_spanned(meta.path, e));
                }

                Ok(())
            });
            match res {
                Err(e) => Some(Err(e)),
                Ok(()) => None,
            }
        })
        .collect::<Result<Vec<_>>>()?;

    Ok(())
}

#[derive(Debug)]
struct MethodInputArgs {
    types: Vec<TokenStream>,
    names: Vec<TokenStream>,
}

impl MethodInputArgs {
    fn parse(method: &ImplItemFn) -> Result<Self> {
        let (in_types, in_names): (Vec<_>, Vec<_>) = method
            .sig
            .inputs
            .iter()
            .filter_map(|arg| match arg {
                syn::FnArg::Typed(arg) => {
                    let arg_type = &arg.ty;
                    let arg_name = match &*arg.pat {
                        syn::Pat::Ident(pat) => &pat.ident,
                        _ => {
                            return Some(Err(syn::Error::new(
                                arg.span(),
                                "Expected identifier as argument name",
                            )))
                        }
                    };

                    Some(Ok((quote! { #arg_type }, quote! { #arg_name })))
                }
                // Filter `self`.
                syn::FnArg::Receiver(_) => None,
            })
            .collect::<Result<Vec<_>>>()?
            .into_iter()
            .unzip();

        Ok(Self {
            types: in_types,
            names: in_names,
        })
    }
}
