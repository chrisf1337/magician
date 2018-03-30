use error::{Error, Pos, Result};

pub struct Lexer {
    input: Vec<char>,
    pub index: usize,
    pub row: usize,
    pub col: usize,
    block_comment_start: String,
    block_comment_end: String,
}

impl Lexer {
    pub fn new(input: &str, block_comment_start: &str, block_comment_end: &str) -> Lexer {
        if block_comment_start.len() > 0 && block_comment_end.len() == 0 {
            panic!("cannot have non-empty block comment start and empty block comment end");
        }
        if block_comment_end.len() > 0 && block_comment_start.len() == 0 {
            panic!("cannot have non-empty block comment end and empty block comment start");
        }
        Lexer {
            input: input.chars().collect(),
            index: 0,
            row: 1,
            col: 1,
            block_comment_start: block_comment_start.to_string(),
            block_comment_end: block_comment_end.to_string(),
        }
    }

    pub fn set_pos(&mut self, pos: Pos) {
        self.index = pos.0;
        self.row = pos.1;
        self.col = pos.2;
    }

    pub fn pos(&self) -> Pos {
        (self.index, self.row, self.col)
    }

    pub fn eof(&self) -> bool {
        self.index >= self.input.len()
    }

    fn get_char(&self, i: usize) -> Result<char> {
        if i >= self.input.len() {
            return Err(Error::Eof(self.pos()));
        }
        return Ok(self.input[i]);
    }

    fn get_chars(&self, i: usize, chars: &str) -> Result<()> {
        // println!("get_chars({}, {})", i, chars);
        if chars.len() == 0 {
            panic!("Cannot call get_chars() with empty string");
        }
        let mut res = self.get_char(i);
        let mut off = 0;
        for c in chars.chars() {
            // println!("{}", off);
            // println!("{:?}", res);
            res = res.and_then(|ch| {
                if ch == c {
                    off += 1;
                    if off == chars.len() {
                        Ok(ch)
                    } else {
                        self.get_char(i + off)
                    }
                } else {
                    Err(Error::Unexpected(
                        self.pos(),
                        format!("expected {:?}", chars).to_string(),
                    ))
                }
            });
        }
        match res {
            Ok(_) => Ok(()),
            Err(err) => Err(err),
        }
    }

    pub fn consume_char(&mut self) -> Result<(Pos, char)> {
        // Check for comment
        let mut in_comment = false;
        let start_pos = self.pos();
        if self.block_comment_start.len() > 0 {
            while self.get_chars(self.index, &self.block_comment_start)
                .is_ok()
            {
                // Look for the end of the comment
                in_comment = true;
                let mut found_end = false;
                self.index += self.block_comment_start.len();
                self.col += self.block_comment_start.len();
                while self.index < self.input.len() {
                    if self.get_chars(self.index, &self.block_comment_end).is_ok() {
                        found_end = true;
                        break;
                    } else {
                        match self.get_char(self.index) {
                            Ok('\n') => {
                                self.row += 1;
                                self.col = 1;
                            }
                            _ => self.col += 1,
                        }
                        self.index += 1;
                    }
                }
                if found_end {
                    self.index += self.block_comment_end.len();
                    self.col += self.block_comment_end.len();
                    in_comment = false;
                }
            }
            if in_comment {
                // reached EOF without finding comment close; consume as regular text
                self.set_pos(start_pos);
            }
        }
        match self.get_char(self.index) {
            Ok('\n') => {
                let pos = self.pos();
                self.index += 1;
                self.row += 1;
                self.col = 1;
                Ok((pos, '\n'))
            }
            Ok(ch) => {
                let pos = self.pos();
                self.index += 1;
                self.col += 1;
                Ok((pos, ch))
            }
            Err(err) => Err(err),
        }
    }

    pub fn peek_chars(&mut self, mut n: i32) -> Result<(Pos, String)> {
        let start_pos = self.pos();
        let mut chars = String::new();
        let (pos, ch) = match self.consume_char() {
            Ok(res) => res,
            Err(err) => {
                self.set_pos(start_pos);
                return Err(err);
            }
        };
        chars.push(ch);
        n -= 1;
        while n > 0 {
            let ch = match self.consume_char() {
                Ok((_, ch)) => ch,
                Err(err) => {
                    self.set_pos(start_pos);
                    return Err(err);
                }
            };
            chars.push(ch);
            n -= 1;
        }
        self.set_pos(start_pos);
        Ok((pos, chars))
    }

    pub fn peek_char(&mut self) -> Result<(Pos, char)> {
        let start_pos = self.pos();
        match self.consume_char() {
            Ok((pos, ch)) => {
                self.set_pos(start_pos);
                Ok((pos, ch))
            }
            Err(err) => {
                self.set_pos(start_pos);
                Err(err)
            }
        }
    }

    pub fn consume_whitespace(&mut self) -> Result<()> {
        let mut found_whitespace = false;
        loop {
            match self.peek_char() {
                Ok((pos, ch)) => if ch.is_ascii_whitespace() {
                    match ch {
                        ' ' | '\t' | '\n' => {
                            found_whitespace = true;
                            self.consume_char()?
                        }
                        _ => {
                            return Err(Error::Unexpected(
                                pos,
                                format!("unsupported whitespace char: {}", self.input[self.index])
                                    .to_string(),
                            ))
                        }
                    }
                } else {
                    return Ok(());
                },
                Err(err) => {
                    if !found_whitespace {
                        return Err(err);
                    } else {
                        return Ok(());
                    }
                }
            };
        }
    }

    // Does not advance parser on failure since it peeks first (maybe it should?)
    pub fn try_parse_one_char(&mut self, ch: char) -> Result<Pos> {
        let _ = self.consume_whitespace()?;
        self.try_parse_one_char_strict(ch)
    }

    pub fn try_parse_one_char_strict(&mut self, ch: char) -> Result<Pos> {
        let (pos, c) = self.peek_char()?;
        if c == ch {
            self.consume_char()?;
            Ok(pos)
        } else {
            Err(Error::Unexpected(
                pos,
                format!("expected {}, got {}", ch, self.input[self.index]).to_string(),
            ))
        }
    }

    pub fn parse_chars(&mut self, chars: &str) -> Result<Pos> {
        if chars.is_empty() {
            panic!("Cannot call parse_chars() with empty string");
        }
        let chars: Vec<char> = chars.chars().collect();
        let pos = self.try_parse_one_char(chars[0])?;
        for &c in chars.iter().skip(1) {
            self.try_parse_one_char_strict(c)?;
        }
        Ok(pos)
    }

    pub fn try_parse_chars_list(&mut self, chars_list: Vec<&str>) -> Result<(Pos, String)> {
        if chars_list.len() == 0 {
            panic!("Cannot call parse_chars_list() with empty list");
        }
        for &chars in chars_list.iter() {
            match self.try_parse_chars(chars) {
                Ok(pos) => return Ok((pos, chars.to_string())),
                Err(..) => (),
            }
        }
        Err(Error::Unexpected(
            self.pos(),
            format!("parse_chars_list: expected {:?}", chars_list),
        ))
    }

    pub fn parse_chars_strict(&mut self, chars: &str) -> Result<Pos> {
        if chars.is_empty() {
            panic!("Cannot call parse_chars() with empty string");
        }
        let chars: Vec<char> = chars.chars().collect();
        let pos = self.try_parse_one_char_strict(chars[0])?;
        for &c in chars.iter().skip(1) {
            self.try_parse_one_char_strict(c)?;
        }
        Ok(pos)
    }

    pub fn try_parse_chars(&mut self, chars: &str) -> Result<Pos> {
        let start_pos = self.pos();
        match self.parse_chars(chars) {
            ok @ Ok(_) => ok,
            Err(err) => {
                self.set_pos(start_pos);
                Err(err)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;
    use std::fs::File;
    use std::io::prelude::*;
    use std::path::Path;

    #[test]
    fn test_get_chars1() {
        let lexer = Lexer::new("<!--", "", "");
        let res = lexer.get_chars(0, "<!--");
        assert_eq!(res, Ok(()));
    }

    #[test]
    fn test_consume_char1() {
        let mut lexer = Lexer::new("a", "<!--", "-->");
        let res = lexer.consume_char();
        assert_eq!(res, Ok(((0, 1, 1), 'a')));
        assert_eq!(lexer.pos(), (1, 1, 2));
    }

    #[test]
    fn test_consume_char_eof() {
        let mut lexer = Lexer::new("", "<!--", "-->");
        let res = lexer.consume_char();
        assert_eq!(res, Err(Error::Eof((0, 1, 1))));
        assert_eq!(lexer.pos(), (0, 1, 1));
    }

    #[test]
    fn test_consume_char_eof_with_comment() {
        let mut lexer = Lexer::new("<!---->", "<!--", "-->");
        let res = lexer.consume_char();
        assert_eq!(res, Err(Error::Eof((7, 1, 8))));
        assert_eq!(lexer.pos(), (7, 1, 8));
    }

    #[test]
    fn test_consume_char_with_comment1() {
        let mut lexer = Lexer::new("<!---->\n", "<!--", "-->");
        let res = lexer.consume_char();
        assert_eq!(res, Ok(((7, 1, 8), '\n')));
        assert_eq!(lexer.pos(), (8, 2, 1));
    }

    #[test]
    fn test_consume_char_with_fake_comment() {
        let mut lexer = Lexer::new("<!- --->a", "<!--", "-->");
        let res = lexer.consume_char();
        assert_eq!(res, Ok(((0, 1, 1), '<')));
        assert_eq!(lexer.pos(), (1, 1, 2));
    }

    #[test]
    fn test_consume_char_with_newline_in_comment() {
        let mut lexer = Lexer::new("<!--\n-->a", "<!--", "-->");
        let res = lexer.consume_char();
        assert_eq!(res, Ok(((8, 2, 4), 'a')));
        assert_eq!(lexer.pos(), (9, 2, 5));
    }

    #[test]
    fn test_consume_char_with_unclosed_comment() {
        let mut lexer = Lexer::new("<!-- a", "<!--", "-->");
        let res = lexer.consume_char();
        assert_eq!(res, Ok(((0, 1, 1), '<')));
        assert_eq!(lexer.pos(), (1, 1, 2));
    }

    #[test]
    fn test_peek_char1() {
        let mut lexer = Lexer::new("a", "<!--", "-->");
        let res = lexer.peek_char();
        assert_eq!(res, Ok(((0, 1, 1), 'a')));
    }

    #[test]
    fn test_peek_char_eof() {
        let mut lexer = Lexer::new("", "<!--", "-->");
        let res = lexer.peek_char();
        assert_eq!(res, Err(Error::Eof((0, 1, 1))));
    }

    #[test]
    fn test_peek_char_eof_with_comment() {
        let mut lexer = Lexer::new("<!---->", "<!--", "-->");
        let res = lexer.peek_char();
        assert_eq!(res, Err(Error::Eof((7, 1, 8))));
    }

    #[test]
    fn test_peek_char_with_comment1() {
        let mut lexer = Lexer::new("<!---->a", "<!--", "-->");
        let res = lexer.peek_char();
        assert_eq!(res, Ok(((7, 1, 8), 'a')));
    }

    #[test]
    fn test_peek_char_with_fake_comment() {
        let mut lexer = Lexer::new("<!- --->a", "<!--", "-->");
        let res = lexer.peek_char();
        assert_eq!(res, Ok(((0, 1, 1), '<')));
    }

    #[test]
    fn test_peek_char_with_newline_in_comment() {
        let mut lexer = Lexer::new("<!--\n-->a", "<!--", "-->");
        let res = lexer.peek_char();
        assert_eq!(res, Ok(((8, 2, 4), 'a')));
    }

    #[test]
    fn test_peek_char_with_unclosed_comment() {
        let mut lexer = Lexer::new("<!-- a", "<!--", "-->");
        let res = lexer.peek_char();
        assert_eq!(res, Ok(((0, 1, 1), '<')));
    }

    #[test]
    fn test_consume_whitespace1() {
        let mut lexer = Lexer::new(" ab", "<!--", "-->");
        let res = lexer.consume_whitespace();
        assert_eq!(res, Ok(()));
        assert_eq!(lexer.pos(), (1, 1, 2));
    }

    #[test]
    fn test_consume_whitespace2() {
        let mut lexer = Lexer::new("ab", "<!--", "-->");
        let res = lexer.consume_whitespace();
        assert_eq!(res, Ok(()));
        assert_eq!(lexer.pos(), (0, 1, 1));
    }

    #[test]
    fn test_consume_whitespace_with_comment1() {
        let mut lexer = Lexer::new("<!-- --> ", "<!--", "-->");
        let res = lexer.consume_whitespace();
        assert_eq!(res, Ok(()));
        assert_eq!(lexer.pos(), (9, 1, 10));
    }

    #[test]
    fn test_consume_whitespace_with_comment2() {
        let mut lexer = Lexer::new("<!--\n--> ", "<!--", "-->");
        let res = lexer.consume_whitespace();
        assert_eq!(res, Ok(()));
        assert_eq!(lexer.pos(), (9, 2, 5));
    }

    #[test]
    fn test_parse_chars1() {
        let mut lexer = Lexer::new("  abc", "<!--", "-->");
        let res = lexer.parse_chars("ab");
        assert_eq!(res, Ok((2, 1, 3)));
        assert_eq!(lexer.pos(), (4, 1, 5));
    }

    #[test]
    fn test_parse_chars2() {
        let mut lexer = Lexer::new("  a", "<!--", "-->");
        let res = lexer.parse_chars("ab");
        assert_eq!(res, Err(Error::Eof((3, 1, 4))));
        assert_eq!(lexer.pos(), (3, 1, 4));
    }

    #[test]
    fn test_parse_chars_with_comments() {
        let mut lexer = Lexer::new(" <!-- --> a<!-- -->bc", "<!--", "-->");
        let res = lexer.parse_chars("ab");
        assert_eq!(res, Ok((10, 1, 11)));
        assert_eq!(lexer.pos(), (20, 1, 21));
    }

    #[test]
    fn test_parse_chars_with_consecutive_comments1() {
        let mut lexer = Lexer::new(" <!-- --> <!-- -->a<!-- -->bc", "<!--", "-->");
        let res = lexer.parse_chars("ab");
        assert_eq!(res, Ok((18, 1, 19)));
        assert_eq!(lexer.pos(), (28, 1, 29));
    }

    #[test]
    fn test_parse_chars_with_consecutive_comments2() {
        let mut lexer = Lexer::new(" /*-- -*/ /*-- -*/a/*-- -*/bc", "/*", "*/");
        let res = lexer.parse_chars("ab");
        assert_eq!(res, Ok((18, 1, 19)));
        assert_eq!(lexer.pos(), (28, 1, 29));
    }

    #[test]
    fn test_parse_chars_list() {
        let mut lexer = Lexer::new(" abc", "", "");
        let res = lexer.try_parse_chars_list(vec!["123", "ab"]);
        assert_eq!(res, Ok(((1, 1, 2), "ab".to_string())));
        assert_eq!(lexer.pos(), (3, 1, 4));
    }

    #[test]
    fn test_parse_chars_list_fail() {
        let mut lexer = Lexer::new(" abc", "", "");
        let res = lexer.try_parse_chars_list(vec!["123", "bc"]);
        assert_eq!(
            res,
            Err(Error::Unexpected(
                (0, 1, 1),
                "parse_chars_list: expected [\"123\", \"bc\"]".to_string()
            ))
        );
        assert_eq!(lexer.pos(), (0, 1, 1));
    }
}
