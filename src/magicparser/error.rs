use magicparser::Pos;
use std::marker;
use std::result;

#[derive(Debug, Eq, PartialEq)]
pub enum Error {
    Eof(Pos),
    Unexpected(Pos, String),
    Multiple(Vec<Error>),
}

pub trait MultipleErrors<E>
where
    Self: marker::Sized,
{
    fn combine(errs: Vec<Self>) -> E;
}

impl MultipleErrors<Error> for Error {
    fn combine(errs: Vec<Error>) -> Error {
        Error::Multiple(errs)
    }
}

pub type Result<T> = result::Result<T, Error>;
