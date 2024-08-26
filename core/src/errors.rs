use nom::error::{ErrorKind, ParseError};

#[derive(Debug, PartialEq)]
pub enum CustomError<I> {
    VariableDoesNotExist(String),
    CustomError(String),
    Nom(I, ErrorKind),
}

impl<I> From<String> for CustomError<I> {
    fn from(s: String) -> Self {
        Self::CustomError(s)
    }
}

impl<I> From<&str> for CustomError<I> {
    fn from(s: &str) -> Self {
        Self::CustomError(s.to_string())
    }
}

impl<I> ParseError<I> for CustomError<I> {
    fn from_error_kind(input: I, kind: ErrorKind) -> Self {
        CustomError::Nom(input, kind)
    }

    fn append(_: I, _: ErrorKind, other: Self) -> Self {
        other
    }
}
