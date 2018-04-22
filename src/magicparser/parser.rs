use magicparser::error::{Error, MultipleErrors};
use magicparser::lexer::Lexer;
use magicparser::{Pos, Token};
use std::convert::From;

pub trait Parser<E: From<Error> + MultipleErrors<Error>> {
    fn lexer(&mut self) -> &mut Lexer;
    fn lexer_immut(&self) -> &Lexer;

    fn set_pos(&mut self, pos: Pos) {
        self.lexer().set_pos(pos)
    }

    fn pos(&self) -> Pos {
        self.lexer_immut().pos()
    }

    fn parse_attr_identifier(&mut self) -> Result<Token, E> {
        self.lexer().consume_whitespace()?;
        self.parse_attr_identifier_strict()
    }

    fn parse_attr_identifier_strict(&mut self) -> Result<Token, E> {
        let start_pos = self.pos();
        let mut id: Vec<char> = vec![];
        match self.lexer().peek_char() {
            Ok((_, ch)) => if ch.is_ascii_alphabetic() {
                id.push(ch);
                self.lexer().consume_char()?;
            } else {
                return Err(E::from(Error::Unexpected(
                    start_pos,
                    "expected attribute identifier".to_string(),
                )));
            },
            Err(err) => return Err(E::from(err)),
        }
        while let Ok((_, ch)) = self.lexer().peek_char() {
            if ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' {
                id.push(ch);
                self.lexer().consume_char()?;
            } else {
                break;
            }
        }
        if id.is_empty() {
            Err(E::from(Error::Unexpected(
                start_pos,
                "expected attribute identifier".to_string(),
            )))
        } else {
            Ok(Token::AttrIdentifier(start_pos, id.into_iter().collect()))
        }
    }

    fn parse_elem_identifier(&mut self) -> Result<Token, E> {
        self.lexer().consume_whitespace()?;
        self.parse_elem_identifier_strict()
    }

    // Parser pos *must* be at the start of the element identifier, or an error
    // will be returned
    fn parse_elem_identifier_strict(&mut self) -> Result<Token, E> {
        let start_pos = self.pos();
        let mut id: Vec<char> = vec![];
        match self.lexer().peek_char() {
            Ok((_, ch)) => if ch.is_ascii_alphabetic() {
                id.push(ch);
                self.lexer().consume_char()?;
            } else {
                return Err(E::from(Error::Unexpected(
                    start_pos,
                    "expected element identifier".to_string(),
                )));
            },
            Err(err) => return Err(E::from(err)),
        }
        while let Ok((_, ch)) = self.lexer().peek_char() {
            if ch.is_ascii_alphanumeric() || ch == '-' {
                id.push(ch);
                self.lexer().consume_char()?;
            } else {
                break;
            }
        }
        if id.is_empty() {
            Err(E::from(Error::Unexpected(
                start_pos,
                "expected element identifier".to_string(),
            )))
        } else {
            Ok(Token::ElemIdentifier(start_pos, id.into_iter().collect()))
        }
    }

    // Returned position is the start of the first quote char. String returned
    // in Token::Str enum does not include quotes.
    fn parse_string(&mut self) -> Result<Token, E> {
        self.lexer().consume_whitespace()?;

        let mut st: Vec<char> = vec![];
        let (str_start_pos, quote) = match self.lexer().peek_char() {
            Ok((pos, ch)) => if ch != '\'' && ch != '"' {
                return Err(E::from(Error::Unexpected(
                    pos,
                    "expected quote".to_string(),
                )));
            } else {
                self.lexer().consume_char()?
            },
            Err(err) => return Err(E::from(err)),
        };
        while let Ok((pos, ch)) = self.lexer().peek_char() {
            if ch == '\n' {
                return Err(E::from(Error::Unexpected(
                    pos,
                    "unexpected newline in string".to_string(),
                )));
            } else if ch == quote {
                break;
            } else {
                st.push(self.lexer().consume_char()?.1);
            }
        }

        if self.lexer().eof() {
            Err(E::from(Error::Unexpected(
                self.pos(),
                "unexpected EOF when parsing string".to_string(),
            )))
        } else {
            // end of string
            self.lexer().consume_char()?;
            Ok(Token::Str(str_start_pos, st.into_iter().collect()))
        }
    }

    fn parse_number(&mut self) -> Result<Token, E> {
        self.lexer().consume_whitespace()?;
        self.parse_number_strict()
    }

    fn parse_number_strict(&mut self) -> Result<Token, E> {
        let start_pos = self.pos();
        let mut num: Vec<char> = vec![];
        match self.lexer().peek_char() {
            Ok((_, ch)) => if ch.is_ascii_digit() || ch == '-' || ch == '+' {
                self.lexer().consume_char()?;
                num.push(ch)
            } else {
                return Err(E::from(Error::Unexpected(
                    start_pos,
                    "expected number".to_string(),
                )));
            },
            Err(err) => return Err(E::from(err)),
        }
        while let Ok((_, ch)) = self.lexer().peek_char() {
            if ch.is_ascii_digit() {
                self.lexer().consume_char()?;
                num.push(ch)
            } else {
                break;
            }
        }
        if num.is_empty() {
            Err(E::from(Error::Unexpected(
                start_pos,
                "expected number".to_string(),
            )))
        } else {
            match num.into_iter().collect::<String>().parse::<isize>() {
                Ok(n) => Ok(Token::Number(start_pos, n)),
                Err(_) => Err(E::from(Error::Unexpected(
                    start_pos,
                    "expected number".to_string(),
                ))),
            }
        }
    }

    fn try<T>(&mut self, parser: fn(&mut Self) -> Result<T, E>) -> Result<T, E> {
        let start_pos = self.pos();
        match parser(self) {
            ok @ Ok(_) => ok,
            Err(err) => {
                self.set_pos(start_pos);
                Err(err)
            }
        }
    }

    fn try_parsers_rec<T>(
        &mut self,
        parsers: &[fn(&mut Self) -> Result<T, E>],
        mut errors: Vec<E>,
    ) -> Result<T, Vec<E>> {
        if parsers.is_empty() {
            panic!("cannot call try_parsers_rec() with no parsers!");
        }
        let parser = parsers[0];
        match self.try(parser) {
            Ok(ok) => Ok(ok),
            Err(err) => {
                errors.push(err);
                if parsers.len() == 1 {
                    // just tried last parser and it errored, so return all errors
                    Err(errors)
                } else {
                    self.try_parsers_rec(&parsers[1..], errors)
                }
            }
        }
    }

    fn try_parsers<T>(&mut self, parsers: &[fn(&mut Self) -> Result<T, E>]) -> Result<T, E> {
        if parsers.is_empty() {
            panic!("cannot call try_parsers() with no parsers!");
        }
        match self.try_parsers_rec(parsers, vec![]) {
            Ok(ok) => Ok(ok),
            Err(mut errs) => if errs.len() == 1 {
                Err(errs.remove(0))
            } else {
                Err(E::from(E::combine(errs)))
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    struct TestParser {
        lexer: Lexer,
    }

    impl TestParser {
        fn new(input: &str) -> TestParser {
            TestParser {
                lexer: Lexer::new(input, "<!--", "-->"),
            }
        }
    }

    impl Parser<Error> for TestParser {
        fn lexer(&mut self) -> &mut Lexer {
            &mut self.lexer
        }

        fn lexer_immut(&self) -> &Lexer {
            &self.lexer
        }
    }

    #[test]
    fn test_parse_elem_identifier_strict1() {
        let mut parser = TestParser::new("asdf");
        let res = parser.parse_elem_identifier_strict();
        assert_eq!(
            res,
            Ok(Token::ElemIdentifier((0, 1, 1), "asdf".to_string()))
        );
        assert_eq!(parser.pos(), (4, 1, 5));
    }

    #[test]
    fn test_parse_elem_identifier_strict2() {
        let mut parser = TestParser::new("a1s2f");
        let res = parser.parse_elem_identifier_strict();
        assert_eq!(
            res,
            Ok(Token::ElemIdentifier((0, 1, 1), "a1s2f".to_string()))
        );
        assert_eq!(parser.pos(), (5, 1, 6));
    }

    #[test]
    fn test_parse_attr_identifier1() {
        let mut parser = TestParser::new("asdf");
        let res = parser.parse_attr_identifier();
        assert_eq!(
            res,
            Ok(Token::AttrIdentifier((0, 1, 1), "asdf".to_string()))
        );
        assert_eq!(parser.pos(), (4, 1, 5));
    }

    #[test]
    fn test_parse_attr_identifier2() {
        let mut parser = TestParser::new("a1s2f");
        let res = parser.parse_attr_identifier();
        assert_eq!(
            res,
            Ok(Token::AttrIdentifier((0, 1, 1), "a1s2f".to_string()))
        );
        assert_eq!(parser.pos(), (5, 1, 6));
    }

    #[test]
    fn test_parse_elem_identifier_strict_with_hyphen() {
        let mut parser = TestParser::new("ab-cd");
        let res = parser.parse_elem_identifier_strict();
        assert_eq!(
            res,
            Ok(Token::ElemIdentifier((0, 1, 1), "ab-cd".to_string()))
        );
        assert_eq!(parser.pos(), (5, 1, 6));
    }

    #[test]
    fn test_parse_attr_identifier_with_hyphen() {
        let mut parser = TestParser::new("ab-cd");
        let res = parser.parse_attr_identifier();
        assert_eq!(
            res,
            Ok(Token::AttrIdentifier((0, 1, 1), "ab-cd".to_string()))
        );
        assert_eq!(parser.pos(), (5, 1, 6));
    }

    #[test]
    fn test_parse_attr_identifier_with_hyphen_fail() {
        let mut parser = TestParser::new("-cd");
        let res = parser.parse_attr_identifier();
        assert_eq!(
            res,
            Err(Error::Unexpected(
                (0, 1, 1),
                "expected attribute identifier".to_string()
            ))
        );
        // parser stops at 0 since it peeks (not consumes) to see if the first char is alphabetic
        assert_eq!(parser.pos(), (0, 1, 1));
    }

    #[test]
    fn test_parse_elem_identifier_strict_with_underscore() {
        let mut parser = TestParser::new("a_b");
        let res = parser.parse_elem_identifier_strict();
        assert_eq!(res, Ok(Token::ElemIdentifier((0, 1, 1), "a".to_string())));
        assert_eq!(parser.pos(), (1, 1, 2));
    }

    #[test]
    fn test_parse_attr_identifier_with_underscore() {
        let mut parser = TestParser::new("a_b");
        let res = parser.parse_attr_identifier();
        assert_eq!(res, Ok(Token::AttrIdentifier((0, 1, 1), "a_b".to_string())));
        assert_eq!(parser.pos(), (3, 1, 4));
    }

    #[test]
    fn test_parse_elem_identifier_strict_with_underscore_fail() {
        let mut parser = TestParser::new("_ab");
        let res = parser.parse_elem_identifier_strict();
        assert_eq!(
            res,
            Err(Error::Unexpected(
                (0, 1, 1),
                "expected element identifier".to_string()
            ))
        );
        assert_eq!(parser.pos(), (0, 1, 1));
    }

    #[test]
    fn test_parse_attr_identifier_with_underscore_fail() {
        let mut parser = TestParser::new("_ab");
        let res = parser.parse_attr_identifier();
        assert_eq!(
            res,
            Err(Error::Unexpected(
                (0, 1, 1),
                "expected attribute identifier".to_string()
            ))
        );
        assert_eq!(parser.pos(), (0, 1, 1));
    }

    #[test]
    fn test_parse_attr_identifier_ignores_whitespace() {
        let mut parser = TestParser::new(" asdf");
        let res = parser.parse_attr_identifier();
        assert_eq!(
            res,
            Ok(Token::AttrIdentifier((1, 1, 2), "asdf".to_string()))
        );
        assert_eq!(parser.pos(), (5, 1, 6));
    }

    #[test]
    fn test_parse_attr_identifier_ignores_comments1() {
        let mut parser = TestParser::new("<!-- --> asdf");
        let res = parser.parse_attr_identifier();
        assert_eq!(
            res,
            Ok(Token::AttrIdentifier((9, 1, 10), "asdf".to_string()))
        );
        assert_eq!(parser.pos(), (13, 1, 14));
    }

    #[test]
    fn test_parse_elem_identifier_strict_ignores_comments2() {
        let mut parser = TestParser::new("a<!--\n-->sdf");
        let res = parser.parse_elem_identifier_strict();
        assert_eq!(
            res,
            Ok(Token::ElemIdentifier((0, 1, 1), "asdf".to_string()))
        );
        assert_eq!(parser.pos(), (12, 2, 7));
    }

    #[test]
    fn test_parse_attr_identifier_ignores_comments2() {
        let mut parser = TestParser::new(" a<!--\n-->sdf");
        let res = parser.parse_attr_identifier();
        assert_eq!(
            res,
            Ok(Token::AttrIdentifier((1, 1, 2), "asdf".to_string()))
        );
        assert_eq!(parser.pos(), (13, 2, 7));
    }

    #[test]
    fn test_parse_elem_identifier_strict_fail1() {
        let mut parser = TestParser::new("1sdf");
        let res = parser.parse_elem_identifier_strict();
        assert_eq!(
            res,
            Err(Error::Unexpected(
                (0, 1, 1),
                "expected element identifier".to_string()
            ))
        );
        assert_eq!(parser.pos(), (0, 1, 1));
    }

    #[test]
    fn test_parse_attr_identifier_fail1() {
        let mut parser = TestParser::new("1sdf");
        let res = parser.parse_attr_identifier();
        assert_eq!(
            res,
            Err(Error::Unexpected(
                (0, 1, 1),
                "expected attribute identifier".to_string()
            ))
        );
        assert_eq!(parser.pos(), (0, 1, 1));
    }

    #[test]
    fn test_parse_string_whitespace() {
        let mut parser = TestParser::new("\n'abc'");
        let res = parser.parse_string();
        assert_eq!(res, Ok(Token::Str((1, 2, 1), "abc".to_string())));
        assert_eq!(parser.pos(), (6, 2, 6));
    }

    #[test]
    fn test_parse_string_double_quote() {
        let mut parser = TestParser::new("\"asdf\"");
        let res = parser.parse_string();
        assert_eq!(res, Ok(Token::Str((0, 1, 1), "asdf".to_string())));
        assert_eq!(parser.pos(), (6, 1, 7));
    }

    #[test]
    fn test_parse_string_single_quote() {
        let mut parser = TestParser::new("'asdf'");
        let res = parser.parse_string();
        assert_eq!(res, Ok(Token::Str((0, 1, 1), "asdf".to_string())));
        assert_eq!(parser.pos(), (6, 1, 7));
    }

    #[test]
    fn test_parse_string_fail_no_closing_quote() {
        let mut parser = TestParser::new("'asdf");
        let res = parser.parse_string();
        assert_eq!(
            res,
            Err(Error::Unexpected(
                (5, 1, 6),
                "unexpected EOF when parsing string".to_string()
            ))
        );
        assert_eq!(parser.pos(), (5, 1, 6));
    }

    #[test]
    fn test_parse_string_fail_wrong_closing_quote() {
        let mut parser = TestParser::new("'asdf\"");
        let res = parser.parse_string();
        assert_eq!(
            res,
            Err(Error::Unexpected(
                (6, 1, 7),
                "unexpected EOF when parsing string".to_string()
            ))
        );
        assert_eq!(parser.pos(), (6, 1, 7));
    }

    #[test]
    fn test_parse_string_with_comment() {
        let mut parser = TestParser::new("'a<!-- \n --> df' a");
        let res = parser.parse_string();
        assert_eq!(res, Ok(Token::Str((0, 1, 1), "a df".to_string())));
        assert_eq!(parser.pos(), (16, 2, 9));
    }

    #[test]
    fn test_parse_string_unclosed_with_comment() {
        let mut parser = TestParser::new("'a<!-- \n --> df a");
        let res = parser.parse_string();
        assert_eq!(
            res,
            Err(Error::Unexpected(
                (17, 2, 10),
                "unexpected EOF when parsing string".to_string()
            ))
        );
        assert_eq!(parser.pos(), (17, 2, 10));
    }

    #[test]
    fn test_parse_number_strict1() {
        let mut parser = TestParser::new("123");
        let res = parser.parse_number_strict();
        assert_eq!(res, Ok(Token::Number((0, 1, 1), 123)));
        assert_eq!(parser.pos(), (3, 1, 4));
    }

    #[test]
    fn test_parse_number_strict2() {
        let mut parser = TestParser::new("-1<3");
        let res = parser.parse_number_strict();
        assert_eq!(res, Ok(Token::Number((0, 1, 1), -1)));
        assert_eq!(parser.pos(), (2, 1, 3));
    }

    #[test]
    fn test_parse_number_strict3() {
        let mut parser = TestParser::new("+123");
        let res = parser.parse_number_strict();
        assert_eq!(res, Ok(Token::Number((0, 1, 1), 123)));
        assert_eq!(parser.pos(), (4, 1, 5));
    }

    #[test]
    fn test_parse_number_strict_fail1() {
        let mut parser = TestParser::new("<23");
        let res = parser.parse_number_strict();
        assert_eq!(
            res,
            Err(Error::Unexpected((0, 1, 1), "expected number".to_string()))
        );
        assert_eq!(parser.pos(), (0, 1, 1));
    }

    #[test]
    fn test_parse_number1() {
        let mut parser = TestParser::new(" 123");
        let res = parser.parse_number();
        assert_eq!(res, Ok(Token::Number((1, 1, 2), 123)));
        assert_eq!(parser.pos(), (4, 1, 5));
    }

    #[test]
    fn test_parse_number2() {
        let mut parser = TestParser::new(" -1<3");
        let res = parser.parse_number();
        assert_eq!(res, Ok(Token::Number((1, 1, 2), -1)));
        assert_eq!(parser.pos(), (3, 1, 4));
    }

    #[test]
    fn test_parse_number_fail1() {
        let mut parser = TestParser::new(" <23");
        let res = parser.parse_number();
        assert_eq!(
            res,
            Err(Error::Unexpected((1, 1, 2), "expected number".to_string()))
        );
        assert_eq!(parser.pos(), (1, 1, 2));
    }
}
