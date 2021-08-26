#![allow(incomplete_features)]
#![feature(generic_associated_types)]
#![feature(specialization)]

#[macro_use]
pub mod util;

#[macro_use]
pub mod api;

mod atomic_float;
pub use atomic_float::AtomicFloat;

mod smooth;
pub use smooth::{
    Smooth,
    SmoothOutput,
    SmoothStatus,
};

mod declick;
pub use declick::{
    DeclickParam,
    DeclickOutput
};

pub mod event;
pub use event::Event;

mod float_param;
pub use float_param::{
    SmoothFloatParam,
    SmoothFloatEntry,
    UnsmoothedFloatParam,
    UnsmoothedFloatEntry,
    UIFloatParam,
    UIFloatEntry,
};

mod model;
pub use model::*;

pub mod parameter;
pub use parameter::{Param, ParamInfo};

mod plugin;
pub use plugin::*;

mod time;
pub use time::*;

mod wrapper;
pub use wrapper::UIHostCallback;

pub use baseplug_derive::model;


const MAX_BLOCKSIZE: usize = 128;