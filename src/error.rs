use std::fmt;

use parse::ParseError;

#[derive(Clone, Debug)]
pub struct Error {
    kind: ErrorKind,
}

#[derive(Clone, Debug)]
enum ErrorKind {
    Parse(ParseError),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            ErrorKind::Parse(ref err) => err.fmt(f),
        }
    }
}
