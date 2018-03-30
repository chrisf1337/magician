use error::{Error, Pos, Result};
use lexer::Lexer;

#[derive(Debug, Eq, PartialEq)]
struct SimpleSelector {
    pub element_name: Option<String>,
    pub id: Option<String>,
    pub class: Vec<String>,
}

impl SimpleSelector {
    fn new() -> SimpleSelector {
        SimpleSelector {
            element_name: None,
            id: None,
            class: vec![],
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Selector {
    Simple(SimpleSelector),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    Identifier(Pos, String),
}

pub struct SelectorParser {
    lexer: Lexer,
}

impl SelectorParser {
    fn new(input: &str) -> SelectorParser {
        SelectorParser {
            lexer: Lexer::new(input, "", ""),
        }
    }

    fn pos(&self) -> Pos {
        self.lexer.pos()
    }

    fn parse_identifier(&mut self) -> Result<Token> {
        let _ = self.lexer.consume_whitespace()?;
        self.parse_identifier_strict()
    }

    fn parse_identifier_strict(&mut self) -> Result<Token> {
        let start_pos = self.pos();
        let mut id: Vec<char> = vec![];
        match self.lexer.peek_char() {
            Ok((_, ch)) => if ch.is_ascii_alphabetic() {
                id.push(ch);
                self.lexer.consume_char()?;
            } else {
                return Err(Error::Unexpected(
                    start_pos,
                    "expected identifier".to_string(),
                ));
            },
            Err(err) => return Err(err),
        }
        loop {
            match self.lexer.peek_char() {
                Ok((_, ch)) => if ch.is_ascii_alphanumeric() || ch == '-' {
                    id.push(ch);
                    self.lexer.consume_char()?;
                } else {
                    break;
                },
                Err(_) => break,
            }
        }
        if id.is_empty() {
            Err(Error::Unexpected(
                start_pos,
                "expected identifier".to_string(),
            ))
        } else {
            Ok(Token::Identifier(start_pos, id.into_iter().collect()))
        }
    }

    // fn parse_simple_selector(&mut self) -> Result<Selector> {
    //     let start_pos = self.pos();
    //     let simple_selector = SimpleSelector::new();
    //     match self.peek_char() {
    //         Ok((_, '.')) => {
    //             let class = self.parse_identifier_strict()?;
    //             simple_selector.class.push(class);
    //         }
    //         Ok((_, '#')) => {
    //             let id = self.parse_identifier_strict()?;
    //             simple_selector.
    //         }
    //     }
    // }
}
