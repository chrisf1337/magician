use std::fmt::Debug;

type Pos = (usize, usize, usize); // index, row, col

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Token {
    Str(Pos, String),        // "..." or '...' (no support for quoted entities)
    Identifier(Pos, String), // ascii string starting with a letter
    Value(Pos, String),      // any string not containing whitespace, <, or >
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum NodeType {
    Html,
    Text(String),
    Head,
    Body,
    Img,
    H1,
    P,
}

impl NodeType {
    fn from_tag_id(tag_id_str: &str) -> Option<NodeType> {
        match tag_id_str.to_ascii_lowercase().as_ref() {
            "html" => Some(NodeType::Html),
            "head" => Some(NodeType::Head),
            "body" => Some(NodeType::Body),
            "img" => Some(NodeType::Img),
            "h1" => Some(NodeType::H1),
            "p" => Some(NodeType::P),
            _ => None,
        }
    }

    fn is_void_elem(&self) -> bool {
        match self {
            &NodeType::Img => true,
            _ => false,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct DomNode {
    pub pos: Pos,
    pub node_type: NodeType,
    pub attrs: Vec<(Token, Option<Token>)>,
    pub children: Vec<DomNode>,
}

impl DomNode {
    fn new(
        pos: Pos,
        node_type: NodeType,
        attrs: Vec<(Token, Option<Token>)>,
        children: Vec<DomNode>,
    ) -> DomNode {
        DomNode {
            pos,
            node_type,
            attrs,
            children,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Error {
    Eof(Pos),
    Unexpected(Pos, String),
}

pub struct HtmlParser {
    input: Vec<char>,
    index: usize,
    row: usize,
    col: usize,
}

type ParserResult<T> = Result<T, Error>;
type ParserFn<T> = fn(&mut HtmlParser) -> ParserResult<T>;

impl HtmlParser {
    fn new(input: &str) -> HtmlParser {
        HtmlParser {
            input: input.chars().collect(),
            index: 0,
            row: 1,
            col: 1,
        }
    }

    fn set_pos(&mut self, pos: Pos) {
        self.index = pos.0;
        self.row = pos.1;
        self.col = pos.2;
    }

    fn pos(&self) -> Pos {
        (self.index, self.row, self.col)
    }

    fn get_ch(&self, i: usize) -> ParserResult<char> {
        if i >= self.input.len() {
            return Err(Error::Eof(self.pos()));
        }
        return Ok(self.input[i]);
    }

    fn consume_char(&mut self) -> ParserResult<(Pos, char)> {
        // This code looks bad...
        // Check for comment
        let mut in_comment = false;
        let start_pos = self.pos();
        while self.get_ch(self.index)
            .and_then(|ch| {
                if ch == '<' {
                    self.get_ch(self.index + 1)
                } else {
                    Err(Error::Eof(self.pos()))
                }
            })
            .and_then(|ch| {
                if ch == '!' {
                    self.get_ch(self.index + 2)
                } else {
                    // dummy value; I just need an error here
                    Err(Error::Eof(self.pos()))
                }
            })
            .and_then(|ch| {
                if ch == '-' {
                    self.get_ch(self.index + 3)
                } else {
                    // dummy value; I just need an error here
                    Err(Error::Eof(self.pos()))
                }
            })
            .and_then(|ch| {
                if ch == '-' {
                    Ok(())
                } else {
                    // dummy value; I just need an error here
                    Err(Error::Eof(self.pos()))
                }
            })
            .is_ok()
        {
            // Look for the end of the comment
            in_comment = true;
            let mut found_end = false;
            self.index += 4;
            self.col += 4;
            while self.index < self.input.len() {
                if self.get_ch(self.index)
                    .and_then(|ch| {
                        if ch == '-' {
                            self.get_ch(self.index + 1)
                        } else {
                            Err(Error::Eof(self.pos()))
                        }
                    })
                    .and_then(|ch| {
                        if ch == '-' {
                            self.get_ch(self.index + 2)
                        } else {
                            // dummy value; I just need an error here
                            Err(Error::Eof(self.pos()))
                        }
                    })
                    .and_then(|ch| {
                        if ch == '>' {
                            Ok(())
                        } else {
                            // dummy value; I just need an error here
                            Err(Error::Eof(self.pos()))
                        }
                    })
                    .is_ok()
                {
                    found_end = true;
                    break;
                } else {
                    match self.get_ch(self.index) {
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
                self.index += 3;
                self.col += 3;
                in_comment = false;
            }
        }
        if in_comment {
            // reached EOF without finding comment close; consume as regular text
            self.set_pos(start_pos);
        }
        match self.get_ch(self.index) {
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

    fn peek_chars(&mut self, mut n: i32) -> ParserResult<(Pos, String)> {
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

    fn peek_char(&mut self) -> ParserResult<(Pos, char)> {
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

    fn consume_whitespace(&mut self) -> ParserResult<()> {
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

    fn parse_identifier(&mut self) -> ParserResult<Token> {
        let _ = self.consume_whitespace()?;
        self.parse_identifier_strict()
    }

    fn parse_identifier_strict(&mut self) -> ParserResult<Token> {
        let start_pos = self.pos();
        let mut id: Vec<char> = vec![];
        match self.peek_char() {
            Ok((_, ch)) => if ch.is_ascii_alphabetic() {
                id.push(ch);
                self.consume_char()?;
            } else {
                return Err(Error::Unexpected(
                    start_pos,
                    "expected identifier".to_string(),
                ));
            },
            Err(err) => return Err(err),
        }
        loop {
            match self.peek_char() {
                Ok((_, ch)) => if ch.is_ascii_alphanumeric() || ch == '-' {
                    id.push(ch);
                    self.consume_char()?;
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

    fn parse_value(&mut self) -> ParserResult<Token> {
        let _ = self.consume_whitespace()?;
        let start_pos = self.pos();
        let mut id: Vec<char> = vec![];
        match self.peek_char() {
            Ok((_, ch)) => if ch != '<' && ch != '>' && ch != '"' && ch != '\'' {
                id.push(ch);
                self.consume_char()?;
            } else {
                return Err(Error::Unexpected(start_pos, "expected value".to_string()));
            },
            Err(err) => return Err(err),
        }
        loop {
            match self.peek_char() {
                Ok((_, ch)) => if !ch.is_ascii_whitespace() && ch != '<' && ch != '>' && ch != '"'
                    && ch != '\''
                {
                    id.push(ch);
                    self.consume_char()?;
                } else {
                    break;
                },
                Err(_) => break,
            }
        }
        if id.is_empty() {
            Err(Error::Unexpected(start_pos, "expected value".to_string()))
        } else {
            Ok(Token::Value(start_pos, id.into_iter().collect()))
        }
    }

    // Returned position is the start of the first quote char. String returned in Token::Str enum does not include quotes.
    fn parse_string(&mut self) -> ParserResult<Token> {
        let _ = self.consume_whitespace()?;

        let mut st: Vec<char> = vec![];
        let (str_start_pos, quote) = match self.peek_char() {
            Ok((pos, ch)) => if ch != '\'' && ch != '"' {
                return Err(Error::Unexpected(pos, "expected quote".to_string()));
            } else {
                self.consume_char()?
            },
            Err(err) => return Err(err),
        };
        loop {
            match self.peek_char() {
                Ok((pos, ch)) => if ch == '\n' {
                    return Err(Error::Unexpected(
                        pos,
                        "unexpected newline in string".to_string(),
                    ));
                } else if ch == quote {
                    break;
                } else {
                    st.push(self.consume_char()?.1);
                },
                Err(_) => break,
            }
        }

        if self.index == self.input.len() {
            Err(Error::Unexpected(
                self.pos(),
                "unexpected EOF when parsing string".to_string(),
            ))
        } else {
            // end of string
            self.consume_char()?;
            Ok(Token::Str(str_start_pos, st.into_iter().collect()))
        }
    }

    // Does not advance parser on failure since it peeks first (maybe it should?)
    fn try_parse_one_char(&mut self, ch: char) -> ParserResult<Pos> {
        let _ = self.consume_whitespace()?;
        self.try_parse_one_char_strict(ch)
    }

    fn try_parse_one_char_strict(&mut self, ch: char) -> ParserResult<Pos> {
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

    fn parse_chars(&mut self, chars: &str) -> ParserResult<Pos> {
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

    fn try_parse_chars(&mut self, chars: &str) -> ParserResult<Pos> {
        let start_pos = self.pos();
        match self.parse_chars(chars) {
            ok @ Ok(_) => ok,
            Err(err) => {
                self.set_pos(start_pos);
                Err(err)
            }
        }
    }

    fn try<T>(&mut self, parser: ParserFn<T>) -> ParserResult<T> {
        let start_pos = self.pos();
        match parser(self) {
            ok @ Ok(_) => ok,
            Err(err) => {
                self.set_pos(start_pos);
                Err(err)
            }
        }
    }

    fn try_parse<T: Debug>(&mut self, parsers: &[ParserFn<T>], err_msg: &str) -> ParserResult<T> {
        if parsers.is_empty() {
            return Err(Error::Unexpected(self.pos(), err_msg.to_string()));
        }
        let parser = parsers[0];
        match self.try(parser) {
            ok @ Ok(_) => ok,
            Err(_) => self.try_parse(&parsers[1..], err_msg),
        }
    }

    fn parse_tag_attributes(&mut self) -> ParserResult<Vec<(Token, Option<Token>)>> {
        // roll back to next_start_pos if we parse an incomplete pair
        let mut next_start_pos = self.pos();
        let mut attributes: Vec<(Token, Option<Token>)> = vec![];
        loop {
            match self.parse_identifier() {
                Ok(id) => match self.try_parse_one_char('=') {
                    Ok(_) => {
                        let parsers: Vec<ParserFn<Token>> =
                            vec![Self::parse_string, Self::parse_value];
                        match self.try_parse(&parsers[..], "expected attribute value") {
                            Ok(val_token) => {
                                attributes.push((id, Some(val_token)));
                                next_start_pos = self.pos();
                            }
                            Err(_) => {
                                self.set_pos(next_start_pos);
                                return Ok(attributes);
                            }
                        }
                    }
                    Err(_) => {
                        // no =, so the attribute doesn't have a value
                        attributes.push((id, None));
                        // successfully parsed a pair, so update next_start_pos
                        next_start_pos = self.pos();
                    }
                },
                Err(_) => {
                    return Ok(attributes);
                }
            }
        }
    }

    fn parse_doctype(&mut self) -> ParserResult<()> {
        match self.parse_chars("<!DOCTYPE html>") {
            Ok(_) => Ok(()),
            Err(err) => Err(err),
        }
    }

    fn parse_opening_tag(&mut self) -> ParserResult<DomNode> {
        let tag_start_pos = self.try_parse_one_char('<')?;
        let tag_id = match self.parse_identifier_strict() {
            Ok(tag_id) => tag_id,
            Err(err) => {
                return Err(err);
            }
        };
        let node_type = match tag_id {
            Token::Identifier(tag_id_pos, tag_id_str) => match NodeType::from_tag_id(&tag_id_str) {
                Some(node_type) => node_type,
                None => {
                    return Err(Error::Unexpected(
                        tag_id_pos,
                        format!("unexpected node type: {}", tag_id_str).to_string(),
                    ));
                }
            },
            _ => unreachable!(),
        };
        if node_type.is_void_elem() {
            let attrs = self.parse_tag_attributes()?;
            match self.try_parse_chars("/>") {
                Ok(_) => (),
                Err(_) => match self.try_parse_chars(">") {
                    Ok(_) => (),
                    Err(_) => {
                        return Err(Error::Unexpected(
                            tag_start_pos,
                            format!("unclosed tag: {:?}", node_type),
                        ))
                    }
                },
            };
            Ok(DomNode::new(tag_start_pos, node_type, attrs, vec![]))
        } else {
            let attrs = self.parse_tag_attributes()?;
            match self.try_parse_one_char('>') {
                Ok(_) => (),
                Err(_) => {
                    return Err(Error::Unexpected(
                        tag_start_pos,
                        format!("unclosed tag: {:?}", node_type),
                    ));
                }
            };
            Ok(DomNode::new(tag_start_pos, node_type, attrs, vec![]))
        }
    }

    fn parse_closing_tag(&mut self, opening_tag: DomNode) -> ParserResult<DomNode> {
        let tag_start_pos = self.try_parse_one_char('<')?;
        let _ = self.try_parse_one_char_strict('/')?;
        let tag_id = self.parse_identifier_strict()?;
        let node_type = match tag_id {
            Token::Identifier(tag_id_pos, tag_id_str) => match NodeType::from_tag_id(&tag_id_str) {
                Some(node_type) => node_type,
                None => {
                    return Err(Error::Unexpected(
                        tag_id_pos,
                        format!("unexpected node type: {}", tag_id_str).to_string(),
                    ));
                }
            },
            _ => unreachable!(),
        };
        if opening_tag.node_type != node_type {
            return Err(Error::Unexpected(
                tag_start_pos,
                format!(
                    "expected closing tag for {:?}, got {:?}",
                    opening_tag.node_type, node_type
                ).to_string(),
            ));
        }
        let _ = self.try_parse_one_char('>')?;
        Ok(opening_tag)
    }

    fn parse_text_node(&mut self) -> ParserResult<DomNode> {
        let start_pos = self.pos();
        let mut text: Vec<char> = vec![];
        loop {
            match self.peek_char() {
                Ok((_, ch)) => if ch != '<' {
                    let (_, ch) = self.consume_char()?;
                    text.push(ch);
                } else {
                    break;
                },
                Err(_) => break,
            }
        }
        let s: String = text.into_iter().collect::<String>().trim().to_string();
        if s.len() == 0 {
            Err(Error::Unexpected(self.pos(), "empty text node".to_string()))
        } else {
            Ok(DomNode::new(start_pos, NodeType::Text(s), vec![], vec![]))
        }
    }

    fn parse_node(&mut self) -> ParserResult<DomNode> {
        let mut node = match self.try(HtmlParser::parse_text_node) {
            Ok(node) => node,
            Err(_) => match self.try(HtmlParser::parse_opening_tag) {
                Ok(node) => node,
                Err(err) => {
                    return Err(err);
                }
            },
        };
        if node.node_type.is_void_elem() {
            return Ok(node);
        }
        loop {
            match self.consume_whitespace() {
                Ok(_) => (),
                Err(Error::Eof(_)) => {
                    return Err(Error::Unexpected(
                        node.pos,
                        format!("unclosed element: {:?}", node),
                    ));
                }
                Err(e) => return Err(e),
            };
            if self.index >= self.input.len() {
                return Err(Error::Unexpected(
                    node.pos,
                    format!("unclosed element: {:?}", node),
                ));
            }
            match self.peek_chars(2) {
                Ok((_, chars)) => {
                    if chars == "</" {
                        match self.parse_closing_tag(node.clone()) {
                            ok @ Ok(_) => return ok,
                            Err(Error::Eof(_)) => {
                                return Err(Error::Unexpected(
                                    node.pos,
                                    format!("unclosed element: {:?}", node),
                                ));
                            }
                            Err(err) => return Err(err),
                        }
                    } else if chars.chars().next().unwrap() == '<' {
                        let child_node = match self.parse_node() {
                            Ok(child_node) => child_node,
                            Err(err) => {
                                return Err(err);
                            }
                        };
                        node.children.push(child_node);
                    } else {
                        let text_node = match self.parse_text_node() {
                            Ok(text_node) => text_node,
                            Err(err) => {
                                return Err(err);
                            }
                        };
                        node.children.push(text_node);
                    }
                }
                Err(Error::Eof(_)) => {
                    return Err(Error::Unexpected(
                        node.pos,
                        format!("unclosed element: {:?}", node),
                    ));
                }
                Err(err) => return Err(err),
            }
        }
    }

    pub fn parse(input: &str) -> Result<DomNode, Error> {
        let mut parser = HtmlParser::new(input);
        let _ = parser.try(HtmlParser::parse_doctype);
        let node = parser.parse_node()?;
        Ok(node)
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
    fn test_consume_char1() {
        let mut parser = HtmlParser::new("a");
        let res = parser.consume_char();
        assert_eq!(res, Ok(((0, 1, 1), 'a')));
        assert_eq!(parser.pos(), (1, 1, 2));
    }

    #[test]
    fn test_consume_char_eof() {
        let mut parser = HtmlParser::new("");
        let res = parser.consume_char();
        assert_eq!(res, Err(Error::Eof((0, 1, 1))));
        assert_eq!(parser.pos(), (0, 1, 1));
    }

    #[test]
    fn test_consume_char_eof_with_comment() {
        let mut parser = HtmlParser::new("<!---->");
        let res = parser.consume_char();
        assert_eq!(res, Err(Error::Eof((7, 1, 8))));
        assert_eq!(parser.pos(), (7, 1, 8));
    }

    #[test]
    fn test_consume_char_with_comment1() {
        let mut parser = HtmlParser::new("<!---->\n");
        let res = parser.consume_char();
        assert_eq!(res, Ok(((7, 1, 8), '\n')));
        assert_eq!(parser.pos(), (8, 2, 1));
    }

    #[test]
    fn test_consume_char_with_fake_comment() {
        let mut parser = HtmlParser::new("<!- --->a");
        let res = parser.consume_char();
        assert_eq!(res, Ok(((0, 1, 1), '<')));
        assert_eq!(parser.pos(), (1, 1, 2));
    }

    #[test]
    fn test_consume_char_with_newline_in_comment() {
        let mut parser = HtmlParser::new("<!--\n-->a");
        let res = parser.consume_char();
        assert_eq!(res, Ok(((8, 2, 4), 'a')));
        assert_eq!(parser.pos(), (9, 2, 5));
    }

    #[test]
    fn test_consume_char_with_unclosed_comment() {
        let mut parser = HtmlParser::new("<!-- a");
        let res = parser.consume_char();
        assert_eq!(res, Ok(((0, 1, 1), '<')));
        assert_eq!(parser.pos(), (1, 1, 2));
    }

    #[test]
    fn test_peek_char1() {
        let mut parser = HtmlParser::new("a");
        let res = parser.peek_char();
        assert_eq!(res, Ok(((0, 1, 1), 'a')));
    }

    #[test]
    fn test_peek_char_eof() {
        let mut parser = HtmlParser::new("");
        let res = parser.peek_char();
        assert_eq!(res, Err(Error::Eof((0, 1, 1))));
    }

    #[test]
    fn test_peek_char_eof_with_comment() {
        let mut parser = HtmlParser::new("<!---->");
        let res = parser.peek_char();
        assert_eq!(res, Err(Error::Eof((7, 1, 8))));
    }

    #[test]
    fn test_peek_char_with_comment1() {
        let mut parser = HtmlParser::new("<!---->a");
        let res = parser.peek_char();
        assert_eq!(res, Ok(((7, 1, 8), 'a')));
    }

    #[test]
    fn test_peek_char_with_fake_comment() {
        let mut parser = HtmlParser::new("<!- --->a");
        let res = parser.peek_char();
        assert_eq!(res, Ok(((0, 1, 1), '<')));
    }

    #[test]
    fn test_peek_char_with_newline_in_comment() {
        let mut parser = HtmlParser::new("<!--\n-->a");
        let res = parser.peek_char();
        assert_eq!(res, Ok(((8, 2, 4), 'a')));
    }

    #[test]
    fn test_peek_char_with_unclosed_comment() {
        let mut parser = HtmlParser::new("<!-- a");
        let res = parser.peek_char();
        assert_eq!(res, Ok(((0, 1, 1), '<')));
    }

    #[test]
    fn test_consume_whitespace1() {
        let mut parser = HtmlParser::new(" ab");
        let res = parser.consume_whitespace();
        assert_eq!(res, Ok(()));
        assert_eq!(parser.pos(), (1, 1, 2));
    }

    #[test]
    fn test_consume_whitespace2() {
        let mut parser = HtmlParser::new("ab");
        let res = parser.consume_whitespace();
        assert_eq!(res, Ok(()));
        assert_eq!(parser.pos(), (0, 1, 1));
    }

    #[test]
    fn test_consume_whitespace_with_comment1() {
        let mut parser = HtmlParser::new("<!-- --> ");
        let res = parser.consume_whitespace();
        assert_eq!(res, Ok(()));
        assert_eq!(parser.pos(), (9, 1, 10));
    }

    #[test]
    fn test_consume_whitespace_with_comment2() {
        let mut parser = HtmlParser::new("<!--\n--> ");
        let res = parser.consume_whitespace();
        assert_eq!(res, Ok(()));
        assert_eq!(parser.pos(), (9, 2, 5));
    }

    #[test]
    fn test_parse_chars1() {
        let mut parser = HtmlParser::new("  abc");
        let res = parser.parse_chars("ab");
        assert_eq!(res, Ok((2, 1, 3)));
        assert_eq!(parser.pos(), (4, 1, 5));
    }

    #[test]
    fn test_parse_chars2() {
        let mut parser = HtmlParser::new("  a");
        let res = parser.parse_chars("ab");
        assert_eq!(res, Err(Error::Eof((3, 1, 4))));
        assert_eq!(parser.pos(), (3, 1, 4));
    }

    #[test]
    fn test_parse_chars_with_comments() {
        let mut parser = HtmlParser::new(" <!-- --> a<!-- -->bc");
        let res = parser.parse_chars("ab");
        assert_eq!(res, Ok((10, 1, 11)));
        assert_eq!(parser.pos(), (20, 1, 21));
    }

    #[test]
    fn test_parse_identifier1() {
        let mut parser = HtmlParser::new("asdf");
        let res = parser.parse_identifier();
        assert_eq!(res, Ok(Token::Identifier((0, 1, 1), "asdf".to_string())));
        assert_eq!(parser.pos(), (4, 1, 5));
    }

    #[test]
    fn test_parse_identifier2() {
        let mut parser = HtmlParser::new("a1s2f");
        let res = parser.parse_identifier();
        assert_eq!(res, Ok(Token::Identifier((0, 1, 1), "a1s2f".to_string())));
        assert_eq!(parser.pos(), (5, 1, 6));
    }

    #[test]
    fn test_parse_identifier_with_hyphen() {
        let mut parser = HtmlParser::new("ab-cd");
        let res = parser.parse_identifier();
        assert_eq!(res, Ok(Token::Identifier((0, 1, 1), "ab-cd".to_string())));
        assert_eq!(parser.pos(), (5, 1, 6));
    }

    #[test]
    fn test_parse_identifier_with_hyphen_fail() {
        let mut parser = HtmlParser::new("-cd");
        let res = parser.parse_identifier();
        assert_eq!(
            res,
            Err(Error::Unexpected(
                (0, 1, 1),
                "expected identifier".to_string()
            ))
        );
        // parser stops at 0 since it peeks (not consumes) to see if the first char is alphabetic
        assert_eq!(parser.pos(), (0, 1, 1));
    }

    #[test]
    fn test_parse_identifier_ignores_whitespace() {
        let mut parser = HtmlParser::new(" asdf");
        let res = parser.parse_identifier();
        assert_eq!(res, Ok(Token::Identifier((1, 1, 2), "asdf".to_string())));
        assert_eq!(parser.pos(), (5, 1, 6));
    }

    #[test]
    fn test_parse_identifier_ignores_comments1() {
        let mut parser = HtmlParser::new("<!-- --> asdf");
        let res = parser.parse_identifier();
        assert_eq!(res, Ok(Token::Identifier((9, 1, 10), "asdf".to_string())));
        assert_eq!(parser.pos(), (13, 1, 14));
    }

    #[test]
    fn test_parse_identifier_ignores_comments2() {
        let mut parser = HtmlParser::new(" a<!--\n-->sdf");
        let res = parser.parse_identifier();
        assert_eq!(res, Ok(Token::Identifier((1, 1, 2), "asdf".to_string())));
        assert_eq!(parser.pos(), (13, 2, 7));
    }

    #[test]
    fn test_parse_identifier_fail1() {
        let mut parser = HtmlParser::new("1sdf");
        let res = parser.parse_identifier();
        assert_eq!(
            res,
            Err(Error::Unexpected(
                (0, 1, 1),
                "expected identifier".to_string()
            ))
        );
        assert_eq!(parser.pos(), (0, 1, 1));
    }

    #[test]
    fn test_parse_string_whitespace() {
        let mut parser = HtmlParser::new("\n'abc'");
        let res = parser.parse_string();
        assert_eq!(res, Ok(Token::Str((1, 2, 1), "abc".to_string())));
        assert_eq!(parser.pos(), (6, 2, 6));
    }

    #[test]
    fn test_parse_string_double_quote() {
        let mut parser = HtmlParser::new("\"asdf\"");
        let res = parser.parse_string();
        assert_eq!(res, Ok(Token::Str((0, 1, 1), "asdf".to_string())));
        assert_eq!(parser.pos(), (6, 1, 7));
    }

    #[test]
    fn test_parse_string_single_quote() {
        let mut parser = HtmlParser::new("'asdf'");
        let res = parser.parse_string();
        assert_eq!(res, Ok(Token::Str((0, 1, 1), "asdf".to_string())));
        assert_eq!(parser.pos(), (6, 1, 7));
    }

    #[test]
    fn test_parse_string_fail_no_closing_quote() {
        let mut parser = HtmlParser::new("'asdf");
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
        let mut parser = HtmlParser::new("'asdf\"");
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
        let mut parser = HtmlParser::new("'a<!-- \n --> df' a");
        let res = parser.parse_string();
        assert_eq!(res, Ok(Token::Str((0, 1, 1), "a df".to_string())));
        assert_eq!(parser.pos(), (16, 2, 9));
    }

    #[test]
    fn test_parse_string_unclosed_with_comment() {
        let mut parser = HtmlParser::new("'a<!-- \n --> df a");
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
    fn test_parse_number1() {
        let mut parser = HtmlParser::new("123");
        let res = parser.parse_value();
        assert_eq!(res, Ok(Token::Value((0, 1, 1), "123".to_string())));
        assert_eq!(parser.pos(), (3, 1, 4));
    }

    #[test]
    fn test_parse_number2() {
        let mut parser = HtmlParser::new("1<3");
        let res = parser.parse_value();
        assert_eq!(res, Ok(Token::Value((0, 1, 1), "1".to_string())));
        assert_eq!(parser.pos(), (1, 1, 2));
    }

    #[test]
    fn test_parse_number_fail1() {
        let mut parser = HtmlParser::new("<23");
        let res = parser.parse_value();
        assert_eq!(
            res,
            Err(Error::Unexpected((0, 1, 1), "expected value".to_string()))
        );
        assert_eq!(parser.pos(), (0, 1, 1));
    }

    #[test]
    fn test_parse_tag_attributes1() {
        let mut parser = HtmlParser::new("a=\"a\" b=\"b\"");
        let res = parser.parse_tag_attributes();
        assert_eq!(
            res,
            Ok(vec![
                (
                    Token::Identifier((0, 1, 1), "a".to_string()),
                    Some(Token::Str((2, 1, 3), "a".to_string())),
                ),
                (
                    Token::Identifier((6, 1, 7), "b".to_string()),
                    Some(Token::Str((8, 1, 9), "b".to_string())),
                ),
            ],)
        );
        assert_eq!(parser.pos(), (11, 1, 12));
    }

    #[test]
    fn test_parse_tag_attributes_multiple_value_types() {
        let test_dir = Path::new(&env::var("CARGO_MANIFEST_DIR").unwrap()).join("htmlparser_tests");
        let mut f = File::open(test_dir.join("parse_tag_attributes_multiple_value_types.html"))
            .expect("file not found");
        let mut input = String::new();
        f.read_to_string(&mut input).expect("read");
        let mut parser = HtmlParser::new(&input);
        let res = parser.parse_tag_attributes();
        assert_eq!(
            res,
            Ok(vec![
                (
                    Token::Identifier((0, 1, 1), "a".to_string()),
                    Some(Token::Str((2, 1, 3), "1".to_string())),
                ),
                (
                    Token::Identifier((6, 2, 1), "b".to_string()),
                    Some(Token::Value((8, 2, 3), "1".to_string())),
                ),
                (
                    Token::Identifier((10, 3, 1), "c".to_string()),
                    Some(Token::Value((12, 3, 3), "a1".to_string())),
                ),
            ],)
        );
        assert_eq!(parser.pos(), (15, 4, 1));
    }

    #[test]
    fn test_parse_tag_attributes_whitespace() {
        let test_dir = Path::new(&env::var("CARGO_MANIFEST_DIR").unwrap()).join("htmlparser_tests");
        let mut f = File::open(test_dir.join("parse_tag_attributes_whitespace.html"))
            .expect("file not found");
        let mut input = String::new();
        f.read_to_string(&mut input).expect("read");
        let mut parser = HtmlParser::new(&input);
        let res = parser.parse_tag_attributes();
        assert_eq!(
            res,
            Ok(vec![
                (
                    Token::Identifier((0, 1, 1), "a".to_string()),
                    Some(Token::Str((6, 1, 7), "a".to_string())),
                ),
                (
                    Token::Identifier((10, 2, 1), "b".to_string()),
                    Some(Token::Str((13, 3, 1), "b".to_string())),
                ),
            ],)
        );
        assert_eq!(parser.pos(), (17, 4, 1));
    }

    #[test]
    fn test_parse_tag_attributes_early_termination1() {
        let mut parser = HtmlParser::new("a='a' <");
        let res = parser.parse_tag_attributes();
        assert_eq!(
            res,
            Ok(vec![
                (
                    Token::Identifier((0, 1, 1), "a".to_string()),
                    Some(Token::Str((2, 1, 3), "a".to_string())),
                ),
            ],)
        );
        assert_eq!(parser.pos(), (6, 1, 7));
    }

    #[test]
    fn test_parse_tag_attributes_early_termination2() {
        let mut parser = HtmlParser::new("a='a <");
        let res = parser.parse_tag_attributes();
        assert_eq!(res, Ok(vec![]));
        assert_eq!(parser.pos(), (0, 1, 1));
    }

    #[test]
    fn test_parse_tag_attributes_early_termination3() {
        let mut parser = HtmlParser::new("a b=");
        let res = parser.parse_tag_attributes();
        assert_eq!(
            res,
            Ok(vec![(Token::Identifier((0, 1, 1), "a".to_string()), None)])
        );
        assert_eq!(parser.pos(), (2, 1, 3));
    }

    #[test]
    fn test_parse_tag_attributes_none_val1() {
        let mut parser = HtmlParser::new("a=a b");
        let res = parser.parse_tag_attributes();
        assert_eq!(
            res,
            Ok(vec![
                (
                    Token::Identifier((0, 1, 1), "a".to_string()),
                    Some(Token::Value((2, 1, 3), "a".to_string())),
                ),
                (Token::Identifier((4, 1, 5), "b".to_string()), None),
            ],)
        );
        assert_eq!(parser.pos(), (5, 1, 6));
    }

    #[test]
    fn test_parse_tag_attributes_none_val2() {
        let mut parser = HtmlParser::new("a b");
        let res = parser.parse_tag_attributes();
        assert_eq!(
            res,
            Ok(vec![
                (Token::Identifier((0, 1, 1), "a".to_string()), None),
                (Token::Identifier((2, 1, 3), "b".to_string()), None),
            ],)
        );
        assert_eq!(parser.pos(), (3, 1, 4));
    }

    #[test]
    fn test_parse_tag_attributes_none_val3() {
        let mut parser = HtmlParser::new("a");
        let res = parser.parse_tag_attributes();
        assert_eq!(
            res,
            Ok(vec![(Token::Identifier((0, 1, 1), "a".to_string()), None)],)
        );
        assert_eq!(parser.pos(), (1, 1, 2));
    }

    #[test]
    fn test_try_parse1() {
        let mut parser = HtmlParser::new("abc");
        let parser_fns: Vec<fn(&mut HtmlParser) -> ParserResult<Token>> =
            vec![HtmlParser::parse_identifier, HtmlParser::parse_value];
        let res = parser.try_parse(&parser_fns[..], "");
        assert_eq!(res, Ok(Token::Identifier((0, 1, 1), "abc".to_string())));
        assert_eq!(parser.pos(), (3, 1, 4));
    }

    #[test]
    fn test_try_parse2() {
        let mut parser = HtmlParser::new("123");
        let parser_fns: Vec<fn(&mut HtmlParser) -> ParserResult<Token>> =
            vec![HtmlParser::parse_identifier, HtmlParser::parse_value];
        let res = parser.try_parse(&parser_fns[..], "");
        assert_eq!(res, Ok(Token::Value((0, 1, 1), "123".to_string())));
        assert_eq!(parser.pos(), (3, 1, 4));
    }

    #[test]
    fn test_try_parse3() {
        let mut parser = HtmlParser::new("<");
        let parser_fns: Vec<fn(&mut HtmlParser) -> ParserResult<Token>> =
            vec![HtmlParser::parse_identifier, HtmlParser::parse_value];
        let res = parser.try_parse(&parser_fns[..], "error message");
        assert_eq!(
            res,
            Err(Error::Unexpected((0, 1, 1), "error message".to_string()))
        );
        assert_eq!(parser.pos(), (0, 1, 1));
    }

    #[test]
    fn test_parse_opening_tag1() {
        let mut parser = HtmlParser::new("<html>");
        let res = parser.parse_opening_tag();
        assert_eq!(
            res,
            Ok(DomNode::new((0, 1, 1), NodeType::Html, vec![], vec![]),)
        );
        assert_eq!(parser.pos(), (6, 1, 7));
    }

    #[test]
    fn test_parse_opening_tag2() {
        let mut parser = HtmlParser::new("<html a=1>");
        let res = parser.parse_opening_tag();
        assert_eq!(
            res,
            Ok(DomNode::new(
                (0, 1, 1),
                NodeType::Html,
                vec![
                    (
                        Token::Identifier((6, 1, 7), "a".to_string()),
                        Some(Token::Value((8, 1, 9), "1".to_string())),
                    ),
                ],
                vec![]
            )),
        );
        assert_eq!(parser.pos(), (10, 1, 11));
    }

    #[test]
    fn test_parse_opening_tag_with_weird_attributes() {
        let mut parser = HtmlParser::new("<html a-1=/home/>");
        let res = parser.parse_opening_tag();
        assert_eq!(
            res,
            Ok(DomNode::new(
                (0, 1, 1),
                NodeType::Html,
                vec![
                    (
                        Token::Identifier((6, 1, 7), "a-1".to_string()),
                        Some(Token::Value((10, 1, 11), "/home/".to_string())),
                    ),
                ],
                vec![]
            )),
        );
        assert_eq!(parser.pos(), (17, 1, 18));
    }

    #[test]
    fn test_parse_opening_tag_invalid_void_element() {
        let mut parser = HtmlParser::new("<html />");
        let res = parser.parse_opening_tag();
        assert_eq!(
            res,
            Err(Error::Unexpected(
                (0, 1, 1),
                format!("unclosed tag: {:?}", NodeType::Html).to_string()
            ))
        );
        assert_eq!(parser.pos(), (6, 1, 7));
    }

    #[test]
    fn test_parse_opening_tag_strict_whitespace() {
        let mut parser = HtmlParser::new("< html>");
        let res = parser.parse_opening_tag();
        assert_eq!(
            res,
            Err(Error::Unexpected(
                (1, 1, 2),
                "expected identifier".to_string()
            ))
        );
        assert_eq!(parser.pos(), (1, 1, 2));
    }

    #[test]
    fn test_parse_opening_tag_unclosed() {
        let mut parser = HtmlParser::new("<body a=1 b=2");
        let res = parser.parse_opening_tag();
        assert_eq!(
            res,
            Err(Error::Unexpected(
                (0, 1, 1),
                format!("unclosed tag: {:?}", NodeType::Body).to_string()
            ))
        );
        assert_eq!(parser.pos(), (13, 1, 14));
    }

    #[test]
    fn test_parse_node1() {
        let mut parser = HtmlParser::new("<html></html>");
        let res = parser.parse_node();
        assert_eq!(
            res,
            Ok(DomNode::new((0, 1, 1), NodeType::Html, vec![], vec![]),)
        );
        assert_eq!(parser.pos(), (13, 1, 14));
    }

    #[test]
    fn test_parse_node_fail1() {
        let mut parser = HtmlParser::new("<html>< /html>");
        let res = parser.parse_node();
        assert_eq!(
            res,
            Err(Error::Unexpected(
                (7, 1, 8),
                "expected identifier".to_string()
            ))
        );
        // parser stops at 6 because it try()s to parse the second < as an opening tag and fails
        assert_eq!(parser.pos(), (6, 1, 7));
    }

    #[test]
    fn test_parse_node_unclosed_tag1() {
        let mut parser = HtmlParser::new("<html><");
        let res = parser.parse_node();
        assert_eq!(
            res,
            Err(Error::Unexpected(
                (0, 1, 1),
                format!(
                    "unclosed element: {:?}",
                    DomNode::new((0, 1, 1), NodeType::Html, vec![], vec![])
                ).to_string()
            ))
        );
        assert_eq!(parser.pos(), (6, 1, 7));
    }

    #[test]
    fn test_parse_node_unclosed_tag2() {
        let mut parser = HtmlParser::new("<html></");
        let res = parser.parse_node();
        assert_eq!(
            res,
            Err(Error::Unexpected(
                (0, 1, 1),
                format!(
                    "unclosed element: {:?}",
                    DomNode::new((0, 1, 1), NodeType::Html, vec![], vec![])
                ).to_string()
            ))
        );
        // parser stops at 8 because it sees </ as the start of a closing tag
        // and enters parse_closing_tag()
        assert_eq!(parser.pos(), (8, 1, 9));
    }

    #[test]
    fn test_parse_text_node1() {
        let mut parser = HtmlParser::new("<html>hello</html>  ");
        let res = parser.parse_node();
        assert_eq!(
            res,
            Ok(DomNode::new(
                (0, 1, 1),
                NodeType::Html,
                vec![],
                vec![
                    DomNode::new(
                        (6, 1, 7),
                        NodeType::Text("hello".to_string()),
                        vec![],
                        vec![],
                    ),
                ]
            ),)
        );
        assert_eq!(parser.pos(), (18, 1, 19));
    }

    #[test]
    fn test_parse_text_node2() {
        let mut parser = HtmlParser::new("<html> hello    </html>");
        let res = parser.parse_node();
        assert_eq!(
            res,
            Ok(DomNode::new(
                (0, 1, 1),
                NodeType::Html,
                vec![],
                vec![
                    DomNode::new(
                        (7, 1, 8),
                        NodeType::Text("hello".to_string()),
                        vec![],
                        vec![],
                    ),
                ]
            ),)
        );
        assert_eq!(parser.pos(), (23, 1, 24));
    }

    #[test]
    fn test_parse_multiple_nested_nodes() {
        let mut parser = HtmlParser::new("<html> hello hello <body></body>  </html>");
        let res = parser.parse_node();
        assert_eq!(
            res,
            Ok(DomNode::new(
                (0, 1, 1),
                NodeType::Html,
                vec![],
                vec![
                    DomNode::new(
                        (7, 1, 8),
                        NodeType::Text("hello hello".to_string()),
                        vec![],
                        vec![],
                    ),
                    DomNode::new((19, 1, 20), NodeType::Body, vec![], vec![]),
                ]
            ),)
        );
        assert_eq!(parser.pos(), (41, 1, 42));
    }

    #[test]
    fn test_parse_multiple_nested_nodes_attrs() {
        let mut parser = HtmlParser::new("<html>  <body a='b'  c=\"d\"></body>  </html>");
        let res = parser.parse_node();
        assert_eq!(
            res,
            Ok(DomNode::new(
                (0, 1, 1),
                NodeType::Html,
                vec![],
                vec![
                    DomNode::new(
                        (8, 1, 9),
                        NodeType::Body,
                        vec![
                            (
                                Token::Identifier((14, 1, 15), "a".to_string()),
                                Some(Token::Str((16, 1, 17), "b".to_string())),
                            ),
                            (
                                Token::Identifier((21, 1, 22), "c".to_string()),
                                Some(Token::Str((23, 1, 24), "d".to_string())),
                            ),
                        ],
                        vec![],
                    ),
                ]
            ),)
        );
        assert_eq!(parser.pos(), (43, 1, 44));
    }

    #[test]
    fn test_parse_fail_eof1() {
        let mut parser = HtmlParser::new("<html");
        let res = parser.parse_node();
        assert_eq!(
            res,
            Err(Error::Unexpected(
                (0, 1, 1),
                format!("unclosed tag: {:?}", NodeType::Html).to_string()
            ))
        );
        assert_eq!(parser.pos(), (0, 1, 1));
    }

    #[test]
    fn test_parse_fail_eof2() {
        let mut parser = HtmlParser::new("<html>");
        let res = parser.parse_node();
        assert_eq!(
            res,
            Err(Error::Unexpected(
                (0, 1, 1),
                format!(
                    "unclosed element: {:?}",
                    DomNode::new((0, 1, 1), NodeType::Html, vec![], vec![])
                )
            ))
        );
        assert_eq!(parser.pos(), (6, 1, 7));
    }

    #[test]
    fn test_parse_fail_mismatched_closing_tag1() {
        let mut parser = HtmlParser::new("<html></body>");
        let res = parser.parse_node();
        assert_eq!(
            res,
            Err(Error::Unexpected(
                (6, 1, 7),
                format!(
                    "expected closing tag for {:?}, got {:?}",
                    NodeType::Html,
                    NodeType::Body
                )
            ))
        );
        // terminates at 12 since we check for paired tags as soon as the
        // identifier for the closing tag has been parsed
        assert_eq!(parser.pos(), (12, 1, 13));
    }

    #[test]
    fn test_parse_fail_mismatched_closing_tag2() {
        let mut parser = HtmlParser::new("<html><body></body><body></body></body>");
        let res = parser.parse_node();
        assert_eq!(
            res,
            Err(Error::Unexpected(
                (32, 1, 33),
                format!(
                    "expected closing tag for {:?}, got {:?}",
                    NodeType::Html,
                    NodeType::Body
                )
            ))
        );
        assert_eq!(parser.pos(), (38, 1, 39));
    }

    #[test]
    fn test_parse_with_comment1() {
        let mut parser = HtmlParser::new("<ht<!-- -->ml></html>  ");
        let res = parser.parse_node();
        assert_eq!(
            res,
            Ok(DomNode::new((0, 1, 1), NodeType::Html, vec![], vec![]))
        );
        assert_eq!(parser.pos(), (21, 1, 22));
    }

    #[test]
    fn test_parse_opening_tag_is_void_elem1() {
        let mut parser = HtmlParser::new("<img src=\"abc\">");
        let res = parser.parse_opening_tag();
        assert_eq!(
            res,
            Ok(DomNode::new(
                (0, 1, 1),
                NodeType::Img,
                vec![
                    (
                        Token::Identifier((5, 1, 6), "src".to_string()),
                        Some(Token::Str((9, 1, 10), "abc".to_string())),
                    ),
                ],
                vec![]
            ))
        );
        assert_eq!(parser.pos(), (15, 1, 16));
    }

    #[test]
    fn test_parse_opening_tag_is_void_elem2() {
        let mut parser = HtmlParser::new("<img src=\"abc\" />");
        let res = parser.parse_opening_tag();
        assert_eq!(
            res,
            Ok(DomNode::new(
                (0, 1, 1),
                NodeType::Img,
                vec![
                    (
                        Token::Identifier((5, 1, 6), "src".to_string()),
                        Some(Token::Str((9, 1, 10), "abc".to_string())),
                    ),
                ],
                vec![]
            ))
        );
        assert_eq!(parser.pos(), (17, 1, 18));
    }

    #[test]
    fn test_parse_opening_tag_is_void_elem_fail1() {
        let mut parser = HtmlParser::new("<img src=\"abc\" / >");
        let res = parser.parse_opening_tag();
        assert_eq!(
            res,
            Err(Error::Unexpected(
                (0, 1, 1),
                "unclosed tag: Img".to_string()
            ))
        );
        // parser stops at 15 because parse_tag_attributes() fails on the last /
        assert_eq!(parser.pos(), (15, 1, 16));
    }

    #[test]
    fn test_parse_opening_tag_is_void_elem_with_comments() {
        let mut parser = HtmlParser::new("<!----><i<!-- -->mg src<!-- -->=\"abc\">");
        let res = parser.parse_opening_tag();
        assert_eq!(
            res,
            Ok(DomNode::new(
                (7, 1, 8),
                NodeType::Img,
                vec![
                    (
                        Token::Identifier((20, 1, 21), "src".to_string()),
                        Some(Token::Str((32, 1, 33), "abc".to_string())),
                    ),
                ],
                vec![]
            ))
        );
        assert_eq!(parser.pos(), (38, 1, 39));
    }

    #[test]
    fn test_parse_with_doctype() {
        let test_dir = Path::new(&env::var("CARGO_MANIFEST_DIR").unwrap()).join("htmlparser_tests");
        let mut f = File::open(test_dir.join("simple.html")).expect("file not found");
        let mut input = String::new();
        f.read_to_string(&mut input).expect("read");
        let res = HtmlParser::parse(&input);
        assert_eq!(
            res,
            Ok(DomNode::new(
                (16, 2, 1),
                NodeType::Html,
                vec![],
                vec![
                    DomNode::new(
                        (23, 3, 1),
                        NodeType::Body,
                        vec![],
                        vec![
                            DomNode::new(
                                (31, 5, 1),
                                NodeType::H1,
                                vec![],
                                vec![
                                    DomNode::new(
                                        (35, 5, 5),
                                        NodeType::Text("My First Heading".to_string()),
                                        vec![],
                                        vec![],
                                    ),
                                ],
                            ),
                            DomNode::new(
                                (58, 7, 1),
                                NodeType::P,
                                vec![],
                                vec![
                                    DomNode::new(
                                        (61, 7, 4),
                                        NodeType::Text("My first paragraph.".to_string()),
                                        vec![],
                                        vec![],
                                    ),
                                ],
                            ),
                        ],
                    ),
                ]
            ))
        );
    }
}
