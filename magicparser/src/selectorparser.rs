use error::{Error, Pos};
use lexer::Lexer;
use std::convert::From;
use std::result;

#[derive(Debug, Eq, PartialEq)]
enum SelectorParserError {
    Eof(Pos),
    Unexpected(Pos, String),
    MultipleIds(Pos, String),
}

impl From<SelectorParserError> for Error {
    fn from(error: SelectorParserError) -> Self {
        match error {
            SelectorParserError::Eof(pos) => Error::Eof(pos),
            SelectorParserError::Unexpected(pos, msg) => Error::Unexpected(pos, msg),
            SelectorParserError::MultipleIds(pos, ids) => {
                Error::Unexpected(pos, format!("{:?}", ids).to_string())
            }
        }
    }
}

impl From<Error> for SelectorParserError {
    fn from(error: Error) -> Self {
        match error {
            Error::Eof(pos) => SelectorParserError::Eof(pos),
            Error::Unexpected(pos, msg) => SelectorParserError::Unexpected(pos, msg),
        }
    }
}

type Result<T> = result::Result<T, SelectorParserError>;

#[derive(Debug, Eq, PartialEq)]
struct SimpleSelector {
    pub element_name: Option<Token>,
    pub id: Option<Token>,
    pub class: Vec<Token>,
    pub universal: bool,
}

impl SimpleSelector {
    fn new() -> SimpleSelector {
        SimpleSelector::new_with(None, None, vec![], false)
    }

    fn new_with(
        element_name: Option<Token>,
        id: Option<Token>,
        class: Vec<Token>,
        universal: bool,
    ) -> SimpleSelector {
        SimpleSelector {
            element_name,
            id,
            class,
            universal,
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
                return Err(SelectorParserError::Unexpected(
                    start_pos,
                    "expected identifier".to_string(),
                ));
            },
            Err(err) => return Err(SelectorParserError::from(err)),
        }
        loop {
            match self.lexer.peek_char() {
                Ok((_, ch)) => if ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' {
                    id.push(ch);
                    self.lexer.consume_char()?;
                } else {
                    break;
                },
                Err(_) => break,
            }
        }
        if id.is_empty() {
            Err(SelectorParserError::Unexpected(
                start_pos,
                "expected identifier".to_string(),
            ))
        } else {
            Ok(Token::Identifier(start_pos, id.into_iter().collect()))
        }
    }

    // CssParser should have trimmed whitespace from the selector string, so we
    // don't have to worry about consuming whitespace.
    fn parse_simple_selector(&mut self) -> Result<Selector> {
        let start_pos = self.pos();
        let mut simple_selector = SimpleSelector::new();
        loop {
            println!("{:?}", self.pos());
            match self.lexer.peek_char() {
                Ok((_, '.')) => {
                    self.lexer.consume_char()?;
                    let class = self.parse_identifier_strict()?;
                    simple_selector.class.push(class);
                }
                Ok((_, '#')) => {
                    self.lexer.consume_char()?;
                    let id = self.parse_identifier_strict()?;
                    match simple_selector.id {
                        Some(old_id) => {
                            return Err(SelectorParserError::MultipleIds(
                                start_pos,
                                format!("{:?} {:?}", old_id, id).to_string(),
                            ))
                        }
                        None => (),
                    }
                    simple_selector.id = Some(id);
                }
                Ok((_, '*')) => {
                    self.lexer.consume_char()?;
                    simple_selector.universal = true;
                }
                Ok((_, _)) => match self.parse_identifier_strict() {
                    Ok(element_name) => simple_selector.element_name = Some(element_name),
                    Err(..) => break,
                },
                Err(..) => break,
            }
        }
        if simple_selector == SimpleSelector::new() {
            // empty selector
            Err(SelectorParserError::Unexpected(
                start_pos,
                "empty simple selector".to_string(),
            ))
        } else {
            Ok(Selector::Simple(simple_selector))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_selector1() {
        let mut parser = SelectorParser::new("abcd");
        let res = parser.parse_simple_selector();
        assert_eq!(
            res,
            Ok(Selector::Simple(SimpleSelector::new_with(
                Some(Token::Identifier((0, 1, 1), "abcd".to_string())),
                None,
                vec![],
                false
            )))
        );
        assert_eq!(parser.pos(), (4, 1, 5))
    }

    #[test]
    fn test_parse_simple_selector2() {
        let mut parser = SelectorParser::new("#id");
        let res = parser.parse_simple_selector();
        assert_eq!(
            res,
            Ok(Selector::Simple(SimpleSelector::new_with(
                None,
                Some(Token::Identifier((1, 1, 2), "id".to_string())),
                vec![],
                false
            )))
        );
        assert_eq!(parser.pos(), (3, 1, 4))
    }

    #[test]
    fn test_parse_simple_selector3() {
        let mut parser = SelectorParser::new(".cl");
        let res = parser.parse_simple_selector();
        assert_eq!(
            res,
            Ok(Selector::Simple(SimpleSelector::new_with(
                None,
                None,
                vec![Token::Identifier((1, 1, 2), "cl".to_string())],
                false
            )))
        );
        assert_eq!(parser.pos(), (3, 1, 4))
    }

    #[test]
    fn test_parse_simple_selector4() {
        let mut parser = SelectorParser::new("*");
        let res = parser.parse_simple_selector();
        assert_eq!(
            res,
            Ok(Selector::Simple(SimpleSelector::new_with(
                None,
                None,
                vec![],
                true
            )))
        );
        assert_eq!(parser.pos(), (1, 1, 2))
    }

    #[test]
    fn test_parse_simple_selector_combined1() {
        let mut parser = SelectorParser::new("ab#id.cl1.cl2");
        let res = parser.parse_simple_selector();
        assert_eq!(
            res,
            Ok(Selector::Simple(SimpleSelector::new_with(
                Some(Token::Identifier((0, 1, 1), "ab".to_string())),
                Some(Token::Identifier((3, 1, 4), "id".to_string())),
                vec![
                    Token::Identifier((6, 1, 7), "cl1".to_string()),
                    Token::Identifier((10, 1, 11), "cl2".to_string()),
                ],
                false
            )))
        );
        assert_eq!(parser.pos(), (13, 1, 14))
    }

    #[test]
    fn test_parse_simple_selector_fail() {
        let mut parser = SelectorParser::new("-ab");
        let res = parser.parse_simple_selector();
        assert_eq!(
            res,
            Err(SelectorParserError::Unexpected(
                (0, 1, 1),
                "empty simple selector".to_string()
            ))
        );
        assert_eq!(parser.pos(), (0, 1, 1))
    }

    #[test]
    fn test_parse_simple_selector_fail_multiple_ids() {
        let mut parser = SelectorParser::new("ab#id1#id2");
        let res = parser.parse_simple_selector();
        assert_eq!(
            res,
            Err(SelectorParserError::MultipleIds(
                (0, 1, 1),
                format!(
                    "{:?} {:?}",
                    Token::Identifier((3, 1, 4), "id1".to_string()),
                    Token::Identifier((7, 1, 8), "id2".to_string())
                ).to_string()
            ))
        );
        assert_eq!(parser.pos(), (10, 1, 11))
    }

    #[test]
    fn test_parse_simple_selector_before_attr_selector() {
        let mut parser = SelectorParser::new("ab#id*.cl1.cl2[attr=val]");
        let res = parser.parse_simple_selector();
        assert_eq!(
            res,
            Ok(Selector::Simple(SimpleSelector::new_with(
                Some(Token::Identifier((0, 1, 1), "ab".to_string())),
                Some(Token::Identifier((3, 1, 4), "id".to_string())),
                vec![
                    Token::Identifier((7, 1, 8), "cl1".to_string()),
                    Token::Identifier((11, 1, 12), "cl2".to_string()),
                ],
                true
            )))
        );
        assert_eq!(parser.pos(), (14, 1, 15))
    }
}
