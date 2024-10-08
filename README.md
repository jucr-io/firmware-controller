<div align="center">

# Firmware Controller

This crate provides a macro named `controller` that makes it easy to decouple interactions between
components in a `no_std` environment.

[Intro](#-intro) •
[Usage](#-usage) •
[Internals](#-internals)

</div>

# Intro

This crate provides a macro named `controller` that makes it easy to write controller logic for 
firmware.

The controller is responsible for control of all the peripherals based on commands it receives from
other parts of the code. It also notifies peers about state changes and events via signals.
This macro generates all the boilerplate code and client-side API for you.

# Usage

It's best described by an example so let's take example of a very simple firmware that controls an 
LED:

```rust,no_run
use firmware_controller::controller;

#[derive(Debug)]
pub enum MyFirmwareError {
  InvalidState,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum State {
    Enabled,
    Disabled,
}

// The controller struct. This is where you define the state of your firmware.
#[controller]
pub struct Controller {
    #[controller(publish)]
    state: State,
    // Other fields. Note: No all of them need to be published.
}

// The controller implementation. This is where you define the logic of your firmware.
#[controller]
impl Controller {
    // The `signal` attribute marks this method signature (note: no implementation body) as a
    // signal, that you can use to notify other parts of your code about specific events.
    #[controller(signal)]
    pub async fn power_error(&self, description: heapless::String<64>);

    pub async fn enable_power(&mut self) -> Result<(), MyFirmwareError> {
        if self.state != State::Disabled {
            return Err(MyFirmwareError::InvalidState);
        }

        // Any other logic you want to run when enabling power.

        self.set_state(State::Enabled).await;
        self.power_error("Dummy error just for the showcase".try_into().unwrap())
            .await;

        Ok(())
    }

    pub async fn disable_power(&mut self) -> Result<(), MyFirmwareError> {
        if self.state != State::Enabled {
            return Err(MyFirmwareError::InvalidState);
        }

        // Any other logic you want to run when enabling power.

        self.set_state(State::Disabled).await;

        Ok(())
    }

    // Method that doesn't return anything.
    pub async fn return_nothing(&self) {
    }
}

#[embassy_executor::main]
async fn main(spawner: embassy_executor::Spawner) {
    let mut controller = Controller::new(State::Disabled);

    // Spawn the client task.
    spawner.spawn(client());

    // Run the controller logic.
    controller.run().await;
}

// This is just a very silly client that keeps flipping the power state every 1 second.
#[embassy_executor::task]
async fn client() {
    use futures::{future::Either, stream::select, StreamExt};
    use embassy_time::{Timer, Duration};

    let mut client = ControllerClient::new();
    // SAFETY: We don't create more than 16 instances so we won't panic.
    let state_changed = ControllerState::new().unwrap().map(Either::Left);
    let error_stream = ControllerPowerError::new().unwrap().map(Either::Right);
    let mut stream = select(state_changed, error_stream);

    client.enable_power().await.unwrap();
    while let Some(event) = stream.next().await {
        match event {
            Either::Left(ControllerStateChanged {
                new: State::Enabled,
                ..
            }) => {
                // This is fine in this very simple example where we've only one client in a single
                // task. In a real-world application, you should ensure that the stream is polled
                // continuously. Otherwise, you might miss notifications.
                Timer::after(Duration::from_secs(1)).await;

                client.disable_power().await.unwrap();
            }
            Either::Left(ControllerStateChanged {
                new: State::Disabled,
                ..
            }) => {
                Timer::after(Duration::from_secs(1)).await;

                client.enable_power().await.unwrap();
            }
            Either::Right(ControllerPowerErrorArgs { description }) => {
                // Do something with the error.
            }
        }
    }
}
```

# Internals

The `controller` macro will generated the following for you:

* A `new` method that takes the fields of the struct as arguments and returns the struct.
* For each `published` field:
  * Setter for this field, named `set_<field-name>` (so`set_state` here), which broadcasts any
    changes made to this field.
  * Two client-side types:
    * struct named `<struct-name><field-name-in-pascal-case>Changed` (so `ControllerStateChanged`
      for `state` field), containing two public fields, named `previous` and `new` fields
      representing the previous and new values of the field, respectively.
    * Type named `<struct-name><field-name-in-pascal-case>` (so `ControllerState` for
      `state` field), which implements `futures::Stream`, yielding each state change as the change
       struct described above.
* `run` method with signature `pub async fn run(&mut self);` which runs the controller logic,
   proxying calls from the client to the implementations here and their return value back to
   the clients (internally via channels). Typically you'd call it at the end of your `main`
   or run it as a task.
* Client-side API for this struct, named `<struct-name>Client` (`ControllerClient` here)
  which provides exactly the same methods (except signal methods) defined in this implementation
  that other parts of the code use to call these methods.
* For each `signal` method:
  * The method body, that broadcasts the signal to all the clients that are listening to it.
  * Two client-side types:
    * struct, named `<struct-name><method-name-in-pascal-case>Args` (`ControllerPowerErrorArgs`
      here), containing all the arguments of this method, as public fields.
    * Type named `<struct-name><method-name-in-pascal-case>` (`ControllerPowerError` here) which
      implements `futures::Stream`, yielding each signal broadcasted as the args struct described
      above.

## Dependencies assumed

The `controller` macro assumes that you have the following dependencies in your `Cargo.toml`:

* `futures` with `async-await` feature enabled.
* `embassy-sync`

## Known limitations & Caveats

* Currently only works as a singleton: you can create multiple instances of the controller but
  if you run them simultaneously, they'll interfere with each others' operation. We hope to remove
  this limitation in the future. Having said that, most firmware applications will only need a
  single controller instance.
* Method args/return type can't be reference types.
* Methods must be async.
* The maximum number of subscribers state change and signal streams is 16. We plan to provide an
  attribute to make this configurable in the future.
* The type of all published fields must implement `Clone` and `Debug`.
* The signal and published fields' streams must be continuely polled. Otherwise notifications will
  be missed.
