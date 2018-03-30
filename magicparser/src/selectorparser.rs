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
type ParserFn<T> = fn(&mut SelectorParser) -> Result<T>;

#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    // ascii string starting with a letter and may contain letters or numbers
    ElementIdentifier(Pos, String),
    // ascii string starting with a letter and may contain letters, numbers, _, or -
    AttrIdentifier(Pos, String),
    // "..." or '...' (no support for quoted entities)
    Str(Pos, String),
}

#[derive(Debug, Eq, PartialEq)]
pub enum AttrSelectorOp {
    Exactly(Pos),            // =
    ExactlyOne(Pos),         // ~=
    ExactlyOrHyphen(Pos),    // |=
    Prefixed(Pos),           // ^=
    Suffixed(Pos),           // $=
    ContainsAtLeastOne(Pos), // *=
}

#[derive(Debug, Eq, PartialEq)]
struct SimpleSelector {
    pub element_name: Option<Token>,
    pub id: Option<Token>,
    pub class: Vec<Token>,
    pub universal: bool,
}

#[derive(Debug, Eq, PartialEq)]
struct AttrSelector {
    pub attr: Token,
    pub op_val: Option<(AttrSelectorOp, Token)>,
    pub case_insensitive: bool,
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

impl AttrSelector {
    fn new(
        attr: Token,
        op_val: Option<(AttrSelectorOp, Token)>,
        case_insensitive: bool,
    ) -> AttrSelector {
        AttrSelector {
            attr,
            op_val,
            case_insensitive,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Selector {
    Simple(SimpleSelector),
    Attr(AttrSelector),
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

    fn set_pos(&mut self, pos: Pos) {
        self.lexer.index = pos.0;
        self.lexer.row = pos.1;
        self.lexer.col = pos.2;
    }

    fn try<T>(&mut self, parser: ParserFn<T>) -> Result<T> {
        let start_pos = self.pos();
        match parser(self) {
            ok @ Ok(_) => ok,
            Err(err) => {
                self.set_pos(start_pos);
                Err(err)
            }
        }
    }

    fn try_parse<T>(&mut self, parsers: &[ParserFn<T>], err_msg: &str) -> Result<T> {
        if parsers.is_empty() {
            return Err(SelectorParserError::Unexpected(
                self.pos(),
                err_msg.to_string(),
            ));
        }
        let parser = parsers[0];
        match self.try(parser) {
            ok @ Ok(_) => ok,
            Err(_) => self.try_parse(&parsers[1..], err_msg),
        }
    }

    fn parse_attr_identifier(&mut self) -> Result<Token> {
        self.lexer.consume_whitespace()?;
        self.parse_attr_identifier_strict()
    }

    fn parse_attr_identifier_strict(&mut self) -> Result<Token> {
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
            Ok(Token::AttrIdentifier(start_pos, id.into_iter().collect()))
        }
    }

    // Parser pos *must* be at the start of the element identifier, or an error
    // will be returned
    fn parse_element_identifier_strict(&mut self) -> Result<Token> {
        let start_pos = self.pos();
        let mut id: Vec<char> = vec![];
        match self.lexer.peek_char() {
            Ok((_, ch)) => if ch.is_ascii_alphabetic() {
                id.push(ch);
                self.lexer.consume_char()?;
            } else {
                return Err(SelectorParserError::Unexpected(
                    start_pos,
                    "expected element identifier".to_string(),
                ));
            },
            Err(err) => return Err(SelectorParserError::from(err)),
        }
        loop {
            match self.lexer.peek_char() {
                Ok((_, ch)) => if ch.is_ascii_alphanumeric() {
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
                "expected element identifier".to_string(),
            ))
        } else {
            Ok(Token::ElementIdentifier(
                start_pos,
                id.into_iter().collect(),
            ))
        }
    }

    // Returned position is the start of the first quote char. String returned
    // in Token::Str enum does not include quotes.
    fn parse_string(&mut self) -> Result<Token> {
        let _ = self.lexer.consume_whitespace()?;

        let mut st: Vec<char> = vec![];
        let (str_start_pos, quote) = match self.lexer.peek_char() {
            Ok((pos, ch)) => if ch != '\'' && ch != '"' {
                return Err(SelectorParserError::Unexpected(
                    pos,
                    "expected quote".to_string(),
                ));
            } else {
                self.lexer.consume_char()?
            },
            Err(err) => return Err(SelectorParserError::from(err)),
        };
        loop {
            match self.lexer.peek_char() {
                Ok((pos, ch)) => if ch == '\n' {
                    return Err(SelectorParserError::Unexpected(
                        pos,
                        "unexpected newline in string".to_string(),
                    ));
                } else if ch == quote {
                    break;
                } else {
                    st.push(self.lexer.consume_char()?.1);
                },
                Err(_) => break,
            }
        }

        if self.lexer.eof() {
            Err(SelectorParserError::Unexpected(
                self.pos(),
                "unexpected EOF when parsing string".to_string(),
            ))
        } else {
            // end of string
            self.lexer.consume_char()?;
            Ok(Token::Str(str_start_pos, st.into_iter().collect()))
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
                    let class = self.parse_attr_identifier_strict()?;
                    simple_selector.class.push(class);
                }
                Ok((_, '#')) => {
                    self.lexer.consume_char()?;
                    let id = self.parse_attr_identifier_strict()?;
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
                Ok((_, _)) => match self.parse_element_identifier_strict() {
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

    fn parse_attr_selector_op(&mut self) -> Result<AttrSelectorOp> {
        self.lexer.consume_whitespace()?;
        match self.lexer
            .try_parse_chars_list(vec!["=", "~=", "|=", "^=", "$=", "*="])
        {
            Ok((pos, op_str)) => Ok(match op_str.as_ref() {
                "=" => AttrSelectorOp::Exactly(pos),
                "~=" => AttrSelectorOp::ExactlyOne(pos),
                "|=" => AttrSelectorOp::ExactlyOrHyphen(pos),
                "^=" => AttrSelectorOp::Prefixed(pos),
                "$=" => AttrSelectorOp::Suffixed(pos),
                "*=" => AttrSelectorOp::ContainsAtLeastOne(pos),
                _ => unreachable!(),
            }),
            Err(err) => Err(SelectorParserError::from(err)),
        }
    }

    fn parse_attr_selector(&mut self) -> Result<Selector> {
        let start_pos = self.pos();
        self.lexer.parse_chars_strict("[")?;
        let attr = self.parse_attr_identifier()?;
        let op = match self.parse_attr_selector_op() {
            Ok(op) => Some(op),
            Err(SelectorParserError::Unexpected(..)) => None,
            Err(err) => return Err(err),
        };
        let val = match op {
            Some(..) => {
                let parsers: Vec<ParserFn<Token>> =
                    vec![Self::parse_attr_identifier, Self::parse_string];
                Some(self.try_parse(&parsers, "expected value or string")?)
            }
            None => None,
        };
        let case_insensitive = self.lexer.try_parse_chars_list(vec!["i", "I"]).is_ok();
        self.lexer.parse_chars("]")?;
        let op_val = match op {
            Some(op) => Some((op, val.unwrap())),
            None => None,
        };
        Ok(Selector::Attr(AttrSelector::new(
            attr,
            op_val,
            case_insensitive,
        )))
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
                Some(Token::ElementIdentifier((0, 1, 1), "abcd".to_string())),
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
                Some(Token::AttrIdentifier((1, 1, 2), "id".to_string())),
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
                vec![Token::AttrIdentifier((1, 1, 2), "cl".to_string())],
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
                Some(Token::ElementIdentifier((0, 1, 1), "ab".to_string())),
                Some(Token::AttrIdentifier((3, 1, 4), "id".to_string())),
                vec![
                    Token::AttrIdentifier((6, 1, 7), "cl1".to_string()),
                    Token::AttrIdentifier((10, 1, 11), "cl2".to_string()),
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
                    Token::AttrIdentifier((3, 1, 4), "id1".to_string()),
                    Token::AttrIdentifier((7, 1, 8), "id2".to_string())
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
                Some(Token::ElementIdentifier((0, 1, 1), "ab".to_string())),
                Some(Token::AttrIdentifier((3, 1, 4), "id".to_string())),
                vec![
                    Token::AttrIdentifier((7, 1, 8), "cl1".to_string()),
                    Token::AttrIdentifier((11, 1, 12), "cl2".to_string()),
                ],
                true
            )))
        );
        assert_eq!(parser.pos(), (14, 1, 15))
    }

    #[test]
    fn test_attribute_selector1() {
        let mut parser = SelectorParser::new("[ abc ]");
        let res = parser.parse_attr_selector();
        assert_eq!(
            res,
            Ok(Selector::Attr(AttrSelector::new(
                Token::AttrIdentifier((2, 1, 3), "abc".to_string()),
                None,
                false,
            )))
        );
        assert_eq!(parser.pos(), (7, 1, 8));
    }

    #[test]
    fn test_attribute_selector2() {
        let mut parser = SelectorParser::new("[ a = b i ]");
        let res = parser.parse_attr_selector();
        assert_eq!(
            res,
            Ok(Selector::Attr(AttrSelector::new(
                Token::AttrIdentifier((2, 1, 3), "a".to_string()),
                Some((
                    AttrSelectorOp::Exactly((4, 1, 5)),
                    Token::AttrIdentifier((6, 1, 7), "b".to_string())
                )),
                true,
            )))
        );
        assert_eq!(parser.pos(), (11, 1, 12));
    }

    #[test]
    fn test_attribute_selector_fail1() {
        let mut parser = SelectorParser::new("[ a = a/ ]");
        let res = parser.parse_attr_selector();
        assert_eq!(
            res,
            Err(SelectorParserError::Unexpected(
                (7, 1, 8),
                "expected ], got /".to_string()
            ))
        );
        assert_eq!(parser.pos(), (7, 1, 8));
    }
}
