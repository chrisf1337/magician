use common::{Pos, Token};
use error::Error;
use lexer::Lexer;
use std::convert::From;

pub trait Parser<E: From<Error>> {
    fn lexer(&mut self) -> &mut Lexer;
    fn lexer_immut(&self) -> &Lexer;

    fn set_pos(&mut self, pos: Pos) {
        self.lexer().index = pos.0;
        self.lexer().row = pos.1;
        self.lexer().col = pos.2;
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
        loop {
            match self.lexer().peek_char() {
                Ok((_, ch)) => if ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' {
                    id.push(ch);
                    self.lexer().consume_char()?;
                } else {
                    break;
                },
                Err(_) => break,
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
        loop {
            match self.lexer().peek_char() {
                Ok((_, ch)) => if ch.is_ascii_alphanumeric() || ch == '-' {
                    id.push(ch);
                    self.lexer().consume_char()?;
                } else {
                    break;
                },
                Err(_) => break,
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
        let _ = self.lexer().consume_whitespace()?;

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
        loop {
            match self.lexer().peek_char() {
                Ok((pos, ch)) => if ch == '\n' {
                    return Err(E::from(Error::Unexpected(
                        pos,
                        "unexpected newline in string".to_string(),
                    )));
                } else if ch == quote {
                    break;
                } else {
                    st.push(self.lexer().consume_char()?.1);
                },
                Err(_) => break,
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

    fn try_parsers<T>(
        &mut self,
        parsers: &[fn(&mut Self) -> Result<T, E>],
        err_msg: &str,
    ) -> Result<T, E> {
        if parsers.is_empty() {
            return Err(E::from(Error::Unexpected(self.pos(), err_msg.to_string())));
        }
        let parser = parsers[0];
        match self.try(parser) {
            ok @ Ok(_) => ok,
            Err(_) => self.try_parsers(&parsers[1..], err_msg),
        }
    }
}
