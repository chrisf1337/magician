#![allow(unknown_lints)]
#![warn(clippy)]

pub mod magicparser;
pub mod style;

#[cfg(test)]
#[macro_use]
extern crate maplit;
#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;
