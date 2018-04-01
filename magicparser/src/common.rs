use std::convert::From;

pub type Pos = (usize, usize, usize); // index, row, col

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token {
    // "..." or '...' (no support for quoted entities)
    Str(Pos, String),
    // ascii string starting with a letter and may contain letters or numbers
    ElemIdentifier(Pos, String),
    // ascii string starting with a letter and may contain letters, numbers, _, or -
    AttrIdentifier(Pos, String),
    // any string not containing whitespace, <, or >
    Value(Pos, String),
    Number(Pos, isize),
}

impl Token {
    pub fn to_lowercase(self) -> Self {
        match self {
            Token::Str(pos, st) => Token::Str(pos, st.to_lowercase()),
            Token::ElemIdentifier(pos, st) => Token::ElemIdentifier(pos, st.to_lowercase()),
            Token::AttrIdentifier(pos, st) => Token::AttrIdentifier(pos, st.to_lowercase()),
            Token::Value(pos, st) => Token::Value(pos, st.to_lowercase()),
            _ => self,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ElemType {
    Html,
    Text(String),
    Head,
    Body,
    Img,
    H1,
    P,
    A,
    Div,
    Custom(String),
}

impl<'a> From<&'a str> for ElemType {
    fn from(tag_id_str: &str) -> Self {
        match tag_id_str.to_ascii_lowercase().as_ref() {
            "html" => ElemType::Html,
            "head" => ElemType::Head,
            "body" => ElemType::Body,
            "img" => ElemType::Img,
            "h1" => ElemType::H1,
            "p" => ElemType::P,
            "a" => ElemType::A,
            "div" => ElemType::Div,
            custom => ElemType::Custom(custom.to_string()),
        }
    }
}

impl<'a> From<&'a String> for ElemType {
    fn from(tag_id_str: &String) -> Self {
        match tag_id_str.to_ascii_lowercase().as_ref() {
            "html" => ElemType::Html,
            "head" => ElemType::Head,
            "body" => ElemType::Body,
            "img" => ElemType::Img,
            "h1" => ElemType::H1,
            "p" => ElemType::P,
            "a" => ElemType::A,
            "div" => ElemType::Div,
            custom => ElemType::Custom(custom.to_string()),
        }
    }
}

impl ElemType {
    pub fn is_void_elem(&self) -> bool {
        match self {
            &ElemType::Img => true,
            _ => false,
        }
    }
}
