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
    Custom(String),
}

impl ElemType {
    pub fn from_str(tag_id_str: &str) -> Option<ElemType> {
        match tag_id_str.to_ascii_lowercase().as_ref() {
            "html" => Some(ElemType::Html),
            "head" => Some(ElemType::Head),
            "body" => Some(ElemType::Body),
            "img" => Some(ElemType::Img),
            "h1" => Some(ElemType::H1),
            "p" => Some(ElemType::P),
            "a" => Some(ElemType::A),
            custom => Some(ElemType::Custom(custom.to_string())),
        }
    }

    pub fn is_void_elem(&self) -> bool {
        match self {
            &ElemType::Img => true,
            _ => false,
        }
    }
}
