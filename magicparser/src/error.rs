use common::Pos;
use std::result;

#[derive(Debug, Eq, PartialEq)]
pub enum Error {
    Eof(Pos),
    Unexpected(Pos, String),
}

pub type Result<T> = result::Result<T, Error>;
