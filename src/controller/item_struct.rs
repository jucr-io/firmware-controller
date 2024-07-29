use crate::util::*;
use proc_macro2::TokenStream;
use quote::quote;
use syn::{spanned::Spanned, Field, Fields, Ident, ItemStruct, Result};

pub(crate) fn expand(mut input: ItemStruct) -> Result<TokenStream> {
    let struct_name = &input.ident;

    let fields = StructFields::parse(&mut input.fields, struct_name)?;
    let field_names = fields.names();
    let (
        publish_channel_declarations,
        publisher_fields_declarations,
        publisher_fields_initializations,
        setters,
        subscriber_declarations,
    ) = fields.published().fold(
        (quote!(), quote!(), quote!(), quote!(), quote!()),
        |(
            publish_channels,
            publisher_fields_declarations,
            publisher_fields_initializations,
            setters,
            subscribers,
        ),
         f| {
            let (publish_channel, publisher_field, publisher_field_init, setter, subscriber) = (
                &f.publish_channel_declaration,
                &f.publisher_field_declaration,
                &f.publisher_field_initialization,
                &f.setter,
                &f.subscriber_declaration,
            );

            (
                quote! { #publish_channels #publish_channel },
                quote! { #publisher_fields_declarations #publisher_field, },
                quote! { #publisher_fields_initializations #publisher_field_init, },
                quote! { #setters #setter },
                quote! { #subscribers #subscriber },
            )
        },
    );
    let fields = fields.raw_fields().collect::<Vec<_>>();
    let vis = &input.vis;

    Ok(quote! {
        #vis struct #struct_name {
            #(#fields),*,
            #publisher_fields_declarations
        }

        impl #struct_name {
            #[allow(clippy::too_many_arguments)]
            pub fn new(#(#fields),*) -> Self {
                Self {
                    #(#field_names),*,
                    #publisher_fields_initializations
                }
            }

            #setters
        }

        #publish_channel_declarations

        #subscriber_declarations
    })
}

/// Parsed struct fields, retuned by `parse_struct_fields`.
#[derive(Debug)]
struct StructFields {
    fields: Vec<StructField>,
}

impl StructFields {
    /// Parse the fields of the struct.
    fn parse(fields: &mut Fields, struct_name: &Ident) -> Result<Self> {
        let fields = match fields {
            Fields::Named(fields) => fields
                .named
                .iter_mut()
                .map(|field| StructField::parse(field, struct_name))
                .collect::<Result<Vec<_>>>()?,
            Fields::Unnamed(_) | Fields::Unit => {
                return Err(syn::Error::new_spanned(
                    fields,
                    "controller struct must have only named fields",
                ))
            }
        };

        Ok(Self { fields })
    }

    /// Names of all the fields.
    fn names(&self) -> impl Iterator<Item = &syn::Ident> {
        // We know the fields are named by the time `self` is constructed.
        self.fields
            .iter()
            .map(|f| f.field().ident.as_ref().unwrap())
    }

    /// All raw fields.
    fn raw_fields(&self) -> impl Iterator<Item = &Field> {
        self.fields.iter().map(StructField::field)
    }

    /// All the published fields.
    fn published(&self) -> impl Iterator<Item = &PublishedField> {
        self.fields.iter().filter_map(|field| match field {
            StructField::Published(published) => Some(published),
            _ => None,
        })
    }
}

/// struct fields.
#[derive(Debug)]
enum StructField {
    /// Private field.
    Private(Field),
    /// Published field.
    Published(PublishedField),
}

impl StructField {
    /// Parse a struct field.
    fn parse(field: &mut Field, struct_name: &Ident) -> Result<StructField> {
        PublishedField::parse(field, struct_name).map(|published| {
            published
                .map(StructField::Published)
                .unwrap_or_else(|| StructField::Private(field.clone()))
        })
    }

    /// Get the field.
    fn field(&self) -> &Field {
        match self {
            Self::Private(field) | Self::Published(PublishedField { field, .. }) => field,
        }
    }
}

#[derive(Debug)]
/// Published field.
struct PublishedField {
    /// Struct fields with the `controller` attributes removed.
    field: Field,
    /// Publisher field declaration.
    publisher_field_declaration: proc_macro2::TokenStream,
    /// Publisher field initialization.
    publisher_field_initialization: proc_macro2::TokenStream,
    /// Field setter.
    setter: proc_macro2::TokenStream,
    /// Publish channel declaration.
    publish_channel_declaration: proc_macro2::TokenStream,
    /// Subscriber struct declaration.
    subscriber_declaration: proc_macro2::TokenStream,
}

impl PublishedField {
    /// Parse a struct field.
    fn parse(field: &mut Field, struct_name: &Ident) -> Result<Option<PublishedField>> {
        let attr = match field
            .attrs
            .iter()
            .find(|attr| attr.path().is_ident("controller"))
        {
            Some(attr) => attr,
            None => return Ok(None),
        };
        attr.parse_nested_meta(|meta| {
            if !meta.path.is_ident("publish") {
                let e = format!(
                    "expected `publish` attribute, found `{}`",
                    meta.path.get_ident().unwrap()
                );

                return Err(syn::Error::new_spanned(attr, e));
            }

            Ok(())
        })?;
        field
            .attrs
            .retain(|attr| !attr.path().is_ident("controller"));
        let struct_name = struct_name.to_string();
        let field_name = field.ident.as_ref().unwrap();
        let field_name_str = field_name.to_string();
        let ty = &field.ty;

        let struct_name_caps = pascal_to_snake_case(&struct_name.to_string()).to_ascii_uppercase();
        let field_name_caps = field_name_str.to_ascii_uppercase();
        let publish_channel_name = Ident::new(
            &format!("{struct_name_caps}_{field_name_caps}_CHANNEL"),
            field.span(),
        );

        let field_name_pascal = snake_to_pascal_case(&field_name_str);
        let subscriber_struct_name =
            Ident::new(&format!("{struct_name}{field_name_pascal}"), field.span());
        let change_struct_name = Ident::new(
            &format!("{struct_name}{field_name_pascal}Changed"),
            field.span(),
        );
        let capacity = super::ALL_CHANNEL_CAPACITY;
        let max_subscribers = super::BROADCAST_MAX_SUBSCRIBERS;
        let max_publishers = super::BROADCAST_MAX_PUBLISHERS;

        let publisher_name = Ident::new(&format!("{field_name_str}_publisher"), field.span());
        let publisher_field_declaration = quote! {
            #publisher_name:
                embassy_sync::pubsub::Publisher<
                    'static,
                    embassy_sync::blocking_mutex::raw::CriticalSectionRawMutex,
                    #change_struct_name,
                    #capacity,
                    #max_subscribers,
                    #max_publishers,
                >
        };
        let publisher_field_initialization = quote! {
            // We only create one publisher so we can't fail.
            #publisher_name: embassy_sync::pubsub::PubSubChannel::publisher(&#publish_channel_name).unwrap()
        };
        let setter_name = Ident::new(&format!("set_{field_name_str}"), field.span());
        let setter = quote! {
            pub async fn #setter_name(&mut self, mut value: #ty) {
                core::mem::swap(&mut self.#field_name, &mut value);

                let change = #change_struct_name {
                    previous: value,
                    new: core::clone::Clone::clone(&self.#field_name),
                };
                embassy_sync::pubsub::publisher::Pub::publish_immediate(
                    &self.#publisher_name,
                    change,
                );
            }
        };

        let publish_channel_declaration = quote! {
            static #publish_channel_name:
                embassy_sync::pubsub::PubSubChannel<
                    embassy_sync::blocking_mutex::raw::CriticalSectionRawMutex,
                    #change_struct_name,
                    #capacity,
                    #max_subscribers,
                    #max_publishers,
                > = embassy_sync::pubsub::PubSubChannel::new();
        };

        let subscriber_declaration = quote! {
            pub struct #subscriber_struct_name {
                subscriber: embassy_sync::pubsub::Subscriber<
                    'static,
                    embassy_sync::blocking_mutex::raw::CriticalSectionRawMutex,
                    #change_struct_name,
                    1,
                    16,
                    1,
                >,
            }

            impl #subscriber_struct_name {
                pub fn new() -> Option<Self> {
                    embassy_sync::pubsub::PubSubChannel::subscriber(&#publish_channel_name)
                        .ok()
                        .map(|subscriber| Self { subscriber })
                }
            }

            impl futures::Stream for #subscriber_struct_name {
                type Item = #change_struct_name;

                fn poll_next(
                    self: core::pin::Pin<&mut Self>,
                    cx: &mut core::task::Context<'_>,
                ) -> core::task::Poll<Option<Self::Item>> {
                    let subscriber = core::pin::Pin::new(&mut *self.get_mut().subscriber);
                    futures::Stream::poll_next(subscriber, cx)
                }
            }

            #[derive(Debug, Clone)]
            pub struct #change_struct_name {
                pub previous: #ty,
                pub new: #ty,
            }
        };

        Ok(Some(PublishedField {
            field: field.clone(),
            publisher_field_declaration,
            publisher_field_initialization,
            setter,
            publish_channel_declaration,
            subscriber_declaration,
        }))
    }
}
