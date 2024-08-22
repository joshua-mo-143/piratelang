struct Error {
    loc: u32,
    error: ParsingError,
}

enum ParsingError {
    VariableDoesNotExist(String),
}
