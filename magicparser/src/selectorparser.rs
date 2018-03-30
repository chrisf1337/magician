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
                Error::Unexpected(pos, format!("{:?}", ids))
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
    EltIdentifier(Pos, String),
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
    pub pos: Pos,
    pub element_name: Option<Token>,
    pub id: Option<Token>,
    pub classes: Vec<Token>,
    pub universal: bool,
}

#[derive(Debug, Eq, PartialEq)]
struct AttrSelector {
    pub pos: Pos,
    pub attr: Token,
    pub op_val: Option<(AttrSelectorOp, Token)>,
    pub case_insensitive: bool,
}

impl SimpleSelector {
    fn new(
        pos: Pos,
        element_name: Option<Token>,
        id: Option<Token>,
        classes: Vec<Token>,
        universal: bool,
    ) -> SimpleSelector {
        SimpleSelector {
            pos,
            element_name,
            id,
            classes,
            universal,
        }
    }
}

impl AttrSelector {
    fn new(
        pos: Pos,
        attr: Token,
        op_val: Option<(AttrSelectorOp, Token)>,
        case_insensitive: bool,
    ) -> AttrSelector {
        AttrSelector {
            pos,
            attr,
            op_val,
            case_insensitive,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Selector {
    Seq(Vec<Selector>),
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

    fn try_parsers<T>(&mut self, parsers: &[ParserFn<T>], err_msg: &str) -> Result<T> {
        if parsers.is_empty() {
            return Err(SelectorParserError::Unexpected(
                self.pos(),
                err_msg.to_string(),
            ));
        }
        let parser = parsers[0];
        match self.try(parser) {
            ok @ Ok(_) => ok,
            Err(_) => self.try_parsers(&parsers[1..], err_msg),
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
                    "expected attribute identifier".to_string(),
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
                "expected attribute identifier".to_string(),
            ))
        } else {
            Ok(Token::AttrIdentifier(start_pos, id.into_iter().collect()))
        }
    }

    // Parser pos *must* be at the start of the element identifier, or an error
    // will be returned
    fn parse_elt_identifier_strict(&mut self) -> Result<Token> {
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
            Ok(Token::EltIdentifier(start_pos, id.into_iter().collect()))
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
    // don't have to worry about consuming whitespace. In effect, this makes
    // this parser strict.
    fn parse_simple_selector(&mut self) -> Result<Selector> {
        let start_pos = self.pos();
        let mut element_name: Option<Token> = None;
        let mut classes = vec![];
        let mut id: Option<Token> = None;
        let mut universal = false;
        let mut found = false;
        loop {
            println!("{:?}", self.pos());
            match self.lexer.peek_char() {
                Ok((_, '.')) => {
                    self.lexer.consume_char()?;
                    let class = self.parse_attr_identifier_strict()?;
                    classes.push(class);
                    found = true;
                }
                Ok((_, '#')) => {
                    self.lexer.consume_char()?;
                    let new_id = self.parse_attr_identifier_strict()?;
                    match id {
                        Some(old_id) => {
                            return Err(SelectorParserError::MultipleIds(
                                start_pos,
                                format!("{:?} {:?}", old_id, new_id),
                            ))
                        }
                        None => (),
                    }
                    id = Some(new_id);
                    found = true;
                }
                Ok((_, '*')) => {
                    self.lexer.consume_char()?;
                    universal = true;
                    found = true;
                }
                Ok((_, _)) => match self.parse_elt_identifier_strict() {
                    Ok(elt_name) => {
                        element_name = Some(elt_name);
                        found = true;
                    }
                    Err(..) => break,
                },
                Err(..) => break,
            }
        }
        if !found {
            // empty selector
            Err(SelectorParserError::Unexpected(
                start_pos,
                "empty simple selector".to_string(),
            ))
        } else {
            Ok(Selector::Simple(SimpleSelector::new(
                start_pos,
                element_name,
                id,
                classes,
                universal,
            )))
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

    // Also strict (must start with [)
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
                Some(self.try_parsers(&parsers, "expected value or string")?)
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
            start_pos,
            attr,
            op_val,
            case_insensitive,
        )))
    }

    fn parse_selector_seq(&mut self) -> Result<Selector> {
        let mut selectors = vec![];
        // first check for a simple selector, which must come first
        match self.parse_simple_selector() {
            Ok(sel) => selectors.push(sel),
            Err(..) => (),
        }
        let parsers: Vec<ParserFn<Selector>> = vec![Self::parse_attr_selector];
        loop {
            let selector = match self.try_parsers(&parsers, "") {
                Ok(sel) => sel,
                Err(SelectorParserError::Unexpected(..)) => break,
                Err(err) => return Err(err),
            };
            selectors.push(selector);
        }
        match self.lexer.peek_char() {
            Ok((_, ch)) => {
                return Err(SelectorParserError::Unexpected(
                    self.pos(),
                    format!(
                        "unexpected character after parsing selector sequence: {:?}",
                        ch
                    ),
                ))
            }
            Err(..) => (),
        }
        Ok(Selector::Seq(selectors))
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
            Ok(Selector::Simple(SimpleSelector::new(
                (0, 1, 1),
                Some(Token::EltIdentifier((0, 1, 1), "abcd".to_string())),
                None,
                vec![],
                false,
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
            Ok(Selector::Simple(SimpleSelector::new(
                (0, 1, 1),
                None,
                Some(Token::AttrIdentifier((1, 1, 2), "id".to_string())),
                vec![],
                false,
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
            Ok(Selector::Simple(SimpleSelector::new(
                (0, 1, 1),
                None,
                None,
                vec![Token::AttrIdentifier((1, 1, 2), "cl".to_string())],
                false,
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
            Ok(Selector::Simple(SimpleSelector::new(
                (0, 1, 1),
                None,
                None,
                vec![],
                true,
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
            Ok(Selector::Simple(SimpleSelector::new(
                (0, 1, 1),
                Some(Token::EltIdentifier((0, 1, 1), "ab".to_string())),
                Some(Token::AttrIdentifier((3, 1, 4), "id".to_string())),
                vec![
                    Token::AttrIdentifier((6, 1, 7), "cl1".to_string()),
                    Token::AttrIdentifier((10, 1, 11), "cl2".to_string()),
                ],
                false,
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
                )
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
            Ok(Selector::Simple(SimpleSelector::new(
                (0, 1, 1),
                Some(Token::EltIdentifier((0, 1, 1), "ab".to_string())),
                Some(Token::AttrIdentifier((3, 1, 4), "id".to_string())),
                vec![
                    Token::AttrIdentifier((7, 1, 8), "cl1".to_string()),
                    Token::AttrIdentifier((11, 1, 12), "cl2".to_string()),
                ],
                true,
            )))
        );
        assert_eq!(parser.pos(), (14, 1, 15))
    }

    #[test]
    fn test_parse_attr_selector1() {
        let mut parser = SelectorParser::new("[ abc ]");
        let res = parser.parse_attr_selector();
        assert_eq!(
            res,
            Ok(Selector::Attr(AttrSelector::new(
                (0, 1, 1),
                Token::AttrIdentifier((2, 1, 3), "abc".to_string()),
                None,
                false,
            )))
        );
        assert_eq!(parser.pos(), (7, 1, 8));
    }

    #[test]
    fn test_parse_attr_selector2() {
        let mut parser = SelectorParser::new("[ a = b i ]");
        let res = parser.parse_attr_selector();
        assert_eq!(
            res,
            Ok(Selector::Attr(AttrSelector::new(
                (0, 1, 1),
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
    fn test_parse_attr_selector_fail1() {
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

    #[test]
    fn test_parse_attr_selector_fail_empty1() {
        let mut parser = SelectorParser::new("[]");
        let res = parser.parse_attr_selector();
        assert_eq!(
            res,
            Err(SelectorParserError::Unexpected(
                (1, 1, 2),
                "expected attribute identifier".to_string()
            ))
        );
        assert_eq!(parser.pos(), (1, 1, 2));
    }

    #[test]
    fn test_parse_selector_seq1() {
        let mut parser = SelectorParser::new("a#id[href I][class~='cl']");
        let res = parser.parse_selector_seq();
        assert_eq!(
            res,
            Ok(Selector::Seq(vec![
                Selector::Simple(SimpleSelector::new(
                    (0, 1, 1),
                    Some(Token::EltIdentifier((0, 1, 1), "a".to_string())),
                    Some(Token::AttrIdentifier((2, 1, 3), "id".to_string())),
                    vec![],
                    false,
                )),
                Selector::Attr(AttrSelector::new(
                    (4, 1, 5),
                    Token::AttrIdentifier((5, 1, 6), "href".to_string()),
                    None,
                    true,
                )),
                Selector::Attr(AttrSelector::new(
                    (12, 1, 13),
                    Token::AttrIdentifier((13, 1, 14), "class".to_string()),
                    Some((
                        AttrSelectorOp::ExactlyOne((18, 1, 19)),
                        Token::Str((20, 1, 21), "cl".to_string()),
                    )),
                    false,
                )),
            ]))
        );
        assert_eq!(parser.pos(), (25, 1, 26));
    }

    #[test]
    fn test_parse_selector_seq2() {
        let mut parser = SelectorParser::new("[href I][class~='cl']");
        let res = parser.parse_selector_seq();
        assert_eq!(
            res,
            Ok(Selector::Seq(vec![
                Selector::Attr(AttrSelector::new(
                    (0, 1, 1),
                    Token::AttrIdentifier((1, 1, 2), "href".to_string()),
                    None,
                    true,
                )),
                Selector::Attr(AttrSelector::new(
                    (8, 1, 9),
                    Token::AttrIdentifier((9, 1, 10), "class".to_string()),
                    Some((
                        AttrSelectorOp::ExactlyOne((14, 1, 15)),
                        Token::Str((16, 1, 17), "cl".to_string()),
                    )),
                    false,
                )),
            ]))
        );
        assert_eq!(parser.pos(), (21, 1, 22));
    }

    #[test]
    fn test_parse_selector_seq_fail1() {
        let mut parser = SelectorParser::new("[href I][class~='cl']a");
        let res = parser.parse_selector_seq();
        assert_eq!(
            res,
            Err(SelectorParserError::Unexpected(
                (21, 1, 22),
                "unexpected character after parsing selector sequence: 'a'".to_string()
            ))
        );
        assert_eq!(parser.pos(), (21, 1, 22));
    }

    #[test]
    fn test_parse_selector_seq_fail2() {
        let mut parser = SelectorParser::new("[href I]*[class~='cl']");
        let res = parser.parse_selector_seq();
        assert_eq!(
            res,
            Err(SelectorParserError::Unexpected(
                (8, 1, 9),
                "unexpected character after parsing selector sequence: '*'".to_string()
            ))
        );
        assert_eq!(parser.pos(), (8, 1, 9));
    }
}
