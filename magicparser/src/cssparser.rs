use error::{Error, Pos, Result};
use lexer::Lexer;

#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    Selector(Pos, String),
    Property(Pos, String),
    Value(Pos, String),
}

pub struct CssParser {
    lexer: Lexer,
}

impl CssParser {
    fn new(input: &str) -> CssParser {
        CssParser {
            lexer: Lexer::new(input, "/*", "*/"),
        }
    }

    fn set_pos(&mut self, pos: Pos) {
        self.lexer.index = pos.0;
        self.lexer.row = pos.1;
        self.lexer.col = pos.2;
    }

    fn pos(&self) -> Pos {
        self.lexer.pos()
    }

    fn parse_selector(&mut self) -> Result<Token> {
        self.lexer.consume_whitespace()?;
        let start_pos = self.pos();
        let mut selector: Vec<char> = vec![];
        loop {
            match self.lexer.peek_char() {
                Ok((_, ch)) => if ch != '{' {
                    selector.push(ch);
                    self.lexer.consume_char()?;
                } else {
                    break;
                },
                Err(_) => break,
            }
        }

        if selector.is_empty() {
            Err(Error::Unexpected(
                start_pos,
                "expected selector".to_string(),
            ))
        } else {
            Ok(Token::Selector(
                start_pos,
                selector.into_iter().collect::<String>().trim().to_string(),
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_selector() {
        let mut parser = CssParser::new(" /* comment */ h1 {}");
        let res = parser.parse_selector();
        assert_eq!(res, Ok(Token::Selector((15, 1, 16), "h1".to_string())));
        assert_eq!(parser.pos(), (18, 1, 19));
    }

    #[test]
    fn test_parse_selector2() {
        let mut parser = CssParser::new("a[href*=\"example\"] { font-size: 2em; }");
        let res = parser.parse_selector();
        assert_eq!(
            res,
            Ok(Token::Selector(
                (0, 1, 1),
                "a[href*=\"example\"]".to_string()
            ))
        );
        assert_eq!(parser.pos(), (19, 1, 20));
    }

    #[test]
    fn test_parse_selector_fail() {
        let mut parser = CssParser::new(" { font-size: 2em; }");
        let res = parser.parse_selector();
        assert_eq!(
            res,
            Err(Error::Unexpected(
                (1, 1, 2),
                "expected selector".to_string()
            ))
        );
        assert_eq!(parser.pos(), (1, 1, 2));
    }
}
