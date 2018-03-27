type Pos = (usize, usize, usize); // index, row, col

#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    Str(Pos, String),        // "..." or '...' (no support for quoted entities)
    Identifier(Pos, String), // ascii string starting with a letter
    Number(Pos, i32),        // number
}

#[derive(Debug, Eq, PartialEq)]
pub enum NodeType {
    Html,
    Text,
    Head,
    Body,
    Img,
}

#[derive(Debug, Eq, PartialEq)]
pub struct DomNode {
    pub node_type: NodeType,
    pub attrs: Vec<(Token, Option<Token>)>,
    pub pos: Pos,
    pub children: Vec<DomNode>,
}

impl DomNode {
    fn new(
        node_type: NodeType,
        attrs: Vec<(Token, Option<Token>)>,
        pos: Pos,
        children: Vec<DomNode>,
    ) -> DomNode {
        DomNode {
            node_type,
            attrs,
            pos,
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

    fn consume_char(&mut self) -> ParserResult<char> {
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
            ok @ Ok('\n') => {
                self.index += 1;
                self.row += 1;
                self.col = 1;
                ok
            }
            ok @ Ok(_) => {
                self.index += 1;
                self.col += 1;
                ok
            }
            Err(err) => Err(err),
        }
    }

    fn peek_char(&self) -> ParserResult<char> {
        // This code looks bad...
        // Check for comment
        let mut in_comment = false;
        let mut index = self.index;
        while self.get_ch(index)
            .and_then(|ch| {
                if ch == '<' {
                    self.get_ch(index + 1)
                } else {
                    Err(Error::Eof(self.pos()))
                }
            })
            .and_then(|ch| {
                if ch == '!' {
                    self.get_ch(index + 2)
                } else {
                    // dummy value; I just need an error here
                    Err(Error::Eof(self.pos()))
                }
            })
            .and_then(|ch| {
                if ch == '-' {
                    self.get_ch(index + 3)
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
            index += 4;
            while index < self.input.len() {
                if self.get_ch(index)
                    .and_then(|ch| {
                        if ch == '-' {
                            self.get_ch(index + 1)
                        } else {
                            Err(Error::Eof(self.pos()))
                        }
                    })
                    .and_then(|ch| {
                        if ch == '-' {
                            self.get_ch(index + 2)
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
                    index += 1;
                }
            }
            if found_end {
                index += 3;
                in_comment = false;
            }
        }
        if in_comment {
            index = self.index;
        }
        self.get_ch(index)
    }

    fn consume_whitespace(&mut self) -> ParserResult<()> {
        let start_pos = self.pos();
        if self.index >= self.input.len() {
            return Err(Error::Eof(start_pos));
        }
        while self.index < self.input.len() && self.input[self.index].is_ascii_whitespace() {
            match self.input[self.index] {
                ' ' | '\t' => self.col += 1,
                '\n' => {
                    self.row += 1;
                    self.col = 1;
                }
                _ => {
                    let cur_pos = self.pos();
                    self.set_pos(start_pos);
                    return Err(Error::Unexpected(
                        cur_pos,
                        format!("unsupported whitespace char: {}", self.input[self.index])
                            .to_string(),
                    ));
                }
            }
            self.index += 1;
        }
        Ok(())
    }

    fn parse_identifier(&mut self) -> ParserResult<Token> {
        let _ = self.consume_whitespace()?;
        self.parse_identifier_strict()
    }

    fn parse_identifier_strict(&mut self) -> ParserResult<Token> {
        let start_pos = self.pos();
        if self.index >= self.input.len() {
            return Err(Error::Eof(self.pos()));
        }
        if !self.input[self.index].is_ascii_alphabetic() {
            return Err(Error::Unexpected(
                self.pos(),
                "expected identifier".to_string(),
            ));
        }
        let mut id: Vec<char> = vec![self.input[self.index]];
        self.index += 1;
        self.col += 1;
        while self.index < self.input.len() && self.input[self.index].is_ascii_alphanumeric() {
            id.push(self.input[self.index]);
            self.index += 1;
            self.col += 1;
        }
        Ok(Token::Identifier(start_pos, id.into_iter().collect()))
    }

    // Returned position is the start of the first quote char. String returned in Token::Str enum does not include quotes.
    fn parse_string(&mut self) -> ParserResult<Token> {
        let start_pos = self.pos();
        let _ = self.consume_whitespace()?;
        if self.index >= self.input.len() {
            return Err(Error::Eof(self.pos()));
        }
        if self.input[self.index] != '\'' && self.input[self.index] != '"' {
            return Err(Error::Unexpected(self.pos(), "expected quote".to_string()));
        }
        let quote = self.input[self.index];
        let mut st: Vec<char> = vec![];
        self.index += 1;
        self.col += 1;
        while self.index < self.input.len() && self.input[self.index] != quote {
            if self.input[self.index] == '\n' {
                // Also include \r?
                let cur_pos = self.pos();
                self.set_pos(start_pos);
                return Err(Error::Unexpected(
                    cur_pos,
                    "unexpected newline in string".to_string(),
                ));
            }
            st.push(self.input[self.index]);
            self.index += 1;
            self.col += 1;
        }
        if self.index == self.input.len() {
            let cur_pos = self.pos();
            self.set_pos(start_pos);
            Err(Error::Unexpected(
                cur_pos,
                "unexpected EOF when parsing string".to_string(),
            ))
        } else {
            // end of string
            self.index += 1;
            self.col += 1;
            Ok(Token::Str(start_pos, st.into_iter().collect()))
        }
    }

    fn parse_number(&mut self) -> ParserResult<Token> {
        let _ = self.consume_whitespace()?;
        if self.index >= self.input.len() {
            return Err(Error::Eof(self.pos()));
        }
        if !self.input[self.index].is_ascii_digit() {
            return Err(Error::Unexpected(self.pos(), "expected number".to_string()));
        }
        let mut id: Vec<char> = vec![self.input[self.index]];
        let start_pos = self.pos();
        self.index += 1;
        self.col += 1;
        while self.index < self.input.len() && self.input[self.index].is_ascii_digit() {
            id.push(self.input[self.index]);
            self.index += 1;
            self.col += 1;
        }
        Ok(Token::Number(
            start_pos,
            id.into_iter().collect::<String>().parse::<i32>().unwrap(),
        ))
    }

    fn parse_one_char(&mut self, ch: char) -> ParserResult<()> {
        let _ = self.consume_whitespace()?;
        self.parse_one_char_strict(ch)
    }

    fn parse_one_char_strict(&mut self, ch: char) -> ParserResult<()> {
        let start_pos = self.pos();
        if self.input[self.index] == ch {
            self.index += 1;
            self.col += 1;
            Ok(())
        } else {
            let cur_pos = self.pos();
            self.set_pos(start_pos);
            Err(Error::Unexpected(
                cur_pos,
                format!("expected {}, got {}", ch, self.input[self.index]).to_string(),
            ))
        }
    }

    fn try_parse<T>(
        &mut self,
        parsers: &[fn(&mut Self) -> ParserResult<T>],
        err_msg: &str,
    ) -> ParserResult<T> {
        if parsers.is_empty() {
            return Err(Error::Unexpected(self.pos(), err_msg.to_string()));
        }
        let parser = parsers[0];
        match parser(self) {
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
                Ok(id) => match self.parse_one_char('=') {
                    Ok(()) => {
                        let parsers: Vec<ParserFn<Token>> = vec![
                            Self::parse_identifier,
                            Self::parse_string,
                            Self::parse_number,
                        ];
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

    fn parse_opening_tag(&mut self) -> ParserResult<DomNode> {
        let start_pos = self.pos();
        let _ = self.parse_one_char('<')?;
        let tag_start_pos = (self.index - 1, self.row, self.col - 1);
        let tag_id = match self.parse_identifier_strict() {
            Ok(tag_id) => tag_id,
            Err(err) => {
                self.set_pos(start_pos);
                return Err(err);
            }
        };
        let node_type = match tag_id {
            Token::Identifier(tag_id_pos, tag_id_str) => match tag_id_str_to_node_type(&tag_id_str)
            {
                Some(node_type) => node_type,
                None => {
                    self.set_pos(start_pos);
                    return Err(Error::Unexpected(
                        tag_id_pos,
                        format!("unexpected node type: {}", tag_id_str).to_string(),
                    ));
                }
            },
            _ => unreachable!(),
        };
        // let self_closing = match node_type {
        //     NodeType::Img => true,
        //     _ => false,
        // };
        let attrs = self.parse_tag_attributes()?;
        match self.parse_one_char('>') {
            Ok(_) => (),
            Err(_) => {
                self.set_pos(start_pos);
                return Err(Error::Unexpected(
                    tag_start_pos,
                    format!("unclosed tag: {:?}", node_type),
                ));
            }
        };
        Ok(DomNode::new(node_type, attrs, tag_start_pos, vec![]))
    }

    fn parse_closing_tag(&mut self, opening_tag: DomNode) -> ParserResult<DomNode> {
        let start_pos = self.pos();
        let _ = self.parse_one_char('<')?;
        let tag_start_pos = (self.index - 1, self.row, self.col - 1);
        let _ = self.parse_one_char_strict('/')?;
        let tag_id = self.parse_identifier_strict()?;
        let node_type = match tag_id {
            Token::Identifier(tag_id_pos, tag_id_str) => match tag_id_str_to_node_type(&tag_id_str)
            {
                Some(node_type) => node_type,
                None => {
                    self.set_pos(start_pos);
                    return Err(Error::Unexpected(
                        tag_id_pos,
                        format!("unexpected node type: {}", tag_id_str).to_string(),
                    ));
                }
            },
            _ => unreachable!(),
        };
        if opening_tag.node_type != node_type {
            self.set_pos(start_pos);
            return Err(Error::Unexpected(
                tag_start_pos,
                format!(
                    "expected closing tag for {:?}, got {:?}",
                    opening_tag.node_type, node_type
                ).to_string(),
            ));
        }
        let _ = self.parse_one_char('>')?;
        Ok(opening_tag)
    }

    fn parse_text_node(&mut self) -> ParserResult<DomNode> {
        let start_pos = self.pos();
        let mut text: Vec<char> = vec![];
        println!("{:?}", self.pos());
        while self.index < self.input.len() && self.input[self.index] != '<' {
            text.push(self.input[self.index]);
            self.index += 1;
            match self.input[self.index] {
                '\n' => {
                    self.row += 1;
                    self.col = 1;
                }
                _ => self.col += 1,
            }
        }
        if text.is_empty() {
            self.set_pos(start_pos);
            Err(Error::Unexpected(self.pos(), "empty text node".to_string()))
        } else {
            Ok(DomNode::new(NodeType::Text, vec![], start_pos, vec![]))
        }
    }

    fn parse_node(&mut self) -> ParserResult<DomNode> {
        let start_pos = self.pos();
        let mut node = match self.parse_text_node() {
            Ok(node) => node,
            Err(_) => match self.parse_opening_tag() {
                Ok(node) => node,
                Err(err) => {
                    self.set_pos(start_pos);
                    return Err(err);
                }
            },
        };
        println!("{:?}", node);
        loop {
            match self.consume_whitespace() {
                Ok(_) => (),
                Err(Error::Eof(_)) => {
                    self.set_pos(start_pos);
                    return Err(Error::Unexpected(
                        node.pos,
                        format!("unclosed element: {:?}", node),
                    ));
                }
                Err(e) => return Err(e),
            };
            if self.index >= self.input.len() {
                self.set_pos(start_pos);
                return Err(Error::Unexpected(
                    node.pos,
                    format!("unclosed element: {:?}", node),
                ));
            }
            match self.input[self.index] {
                '<' => {
                    if self.index + 1 >= self.input.len() {
                        self.set_pos(start_pos);
                        return Err(Error::Unexpected(
                            node.pos,
                            format!("unclosed element: {:?}", node),
                        ));
                    }
                    if self.input[self.index + 1] == '/' {
                        match self.parse_closing_tag(node) {
                            ok @ Ok(_) => return ok,
                            Err(err) => {
                                self.set_pos(start_pos);
                                return Err(err);
                            }
                        }
                    }
                    let child_node = match self.parse_node() {
                        Ok(child_node) => child_node,
                        Err(err) => {
                            self.set_pos(start_pos);
                            return Err(err);
                        }
                    };
                    node.children.push(child_node);
                }
                _ => {
                    let text_node = match self.parse_text_node() {
                        Ok(text_node) => text_node,
                        Err(err) => {
                            self.set_pos(start_pos);
                            return Err(err);
                        }
                    };
                    node.children.push(text_node);
                }
            }
        }
    }

    pub fn parse(input: &str) -> Result<DomNode, Error> {
        let mut parser = HtmlParser::new(input);
        let node = parser.parse_node()?;
        Ok(node)
    }
}

fn tag_id_str_to_node_type(tag_id_str: &str) -> Option<NodeType> {
    match tag_id_str.to_ascii_lowercase().as_ref() {
        "html" => Some(NodeType::Html),
        "head" => Some(NodeType::Head),
        "body" => Some(NodeType::Body),
        _ => None,
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
        assert_eq!(res, Ok('a'));
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
        let mut parser = HtmlParser::new("<!---->a");
        let res = parser.consume_char();
        assert_eq!(res, Ok('a'));
        assert_eq!(parser.pos(), (8, 1, 9));
    }

    #[test]
    fn test_consume_char_with_fake_comment() {
        let mut parser = HtmlParser::new("<!- --->a");
        let res = parser.consume_char();
        assert_eq!(res, Ok('<'));
        assert_eq!(parser.pos(), (1, 1, 2));
    }

    #[test]
    fn test_consume_char_with_newline_in_comment() {
        let mut parser = HtmlParser::new("<!--\n-->a");
        let res = parser.consume_char();
        assert_eq!(res, Ok('a'));
        assert_eq!(parser.pos(), (9, 2, 5));
    }

    #[test]
    fn test_consume_char_with_unclosed_comment() {
        let mut parser = HtmlParser::new("<!-- a");
        let res = parser.consume_char();
        assert_eq!(res, Ok('<'));
        assert_eq!(parser.pos(), (1, 1, 2));
    }

    #[test]
    fn test_peek_char1() {
        let parser = HtmlParser::new("a");
        let res = parser.peek_char();
        assert_eq!(res, Ok('a'));
    }

    #[test]
    fn test_peek_char_eof() {
        let parser = HtmlParser::new("");
        let res = parser.peek_char();
        assert_eq!(res, Err(Error::Eof((0, 1, 1))));
    }

    #[test]
    fn test_peek_char_eof_with_comment() {
        let parser = HtmlParser::new("<!---->");
        let res = parser.peek_char();
        // parser doesn't get mutated, so the pos remains as (0, 1, 1)
        assert_eq!(res, Err(Error::Eof((0, 1, 1))));
    }

    #[test]
    fn test_peek_char_with_comment1() {
        let parser = HtmlParser::new("<!---->a");
        let res = parser.peek_char();
        assert_eq!(res, Ok('a'));
    }

    #[test]
    fn test_peek_char_with_fake_comment() {
        let parser = HtmlParser::new("<!- --->a");
        let res = parser.peek_char();
        assert_eq!(res, Ok('<'));
    }

    #[test]
    fn test_peek_char_with_newline_in_comment() {
        let parser = HtmlParser::new("<!--\n-->a");
        let res = parser.peek_char();
        assert_eq!(res, Ok('a'));
    }

    #[test]
    fn test_peek_char_with_unclosed_comment() {
        let parser = HtmlParser::new("<!-- a");
        let res = parser.peek_char();
        assert_eq!(res, Ok('<'));
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
    fn test_parse_identifier_ignores_whitespace() {
        let mut parser = HtmlParser::new(" asdf");
        let res = parser.parse_identifier();
        assert_eq!(res, Ok(Token::Identifier((1, 1, 2), "asdf".to_string())));
        assert_eq!(parser.pos(), (5, 1, 6));
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
        assert_eq!(parser.pos(), (0, 1, 1));
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
        assert_eq!(parser.pos(), (0, 1, 1));
    }

    #[test]
    fn test_parse_number1() {
        let mut parser = HtmlParser::new("123");
        let res = parser.parse_number();
        assert_eq!(res, Ok(Token::Number((0, 1, 1), 123)));
        assert_eq!(parser.pos(), (3, 1, 4));
    }

    #[test]
    fn test_parse_number2() {
        let mut parser = HtmlParser::new("1b3");
        let res = parser.parse_number();
        assert_eq!(res, Ok(Token::Number((0, 1, 1), 1)));
        assert_eq!(parser.pos(), (1, 1, 2));
    }

    #[test]
    fn test_parse_number_fail1() {
        let mut parser = HtmlParser::new("a23");
        let res = parser.parse_number();
        assert_eq!(
            res,
            Err(Error::Unexpected((0, 1, 1), "expected number".to_string()))
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
        let test_dir =
            Path::new(&env::var("CARGO_MANIFEST_DIR").unwrap()).join("src/htmlparser_tests");
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
                    Some(Token::Number((8, 2, 3), 1)),
                ),
                (
                    Token::Identifier((10, 3, 1), "c".to_string()),
                    Some(Token::Identifier((12, 3, 3), "a1".to_string())),
                ),
            ],)
        );
        assert_eq!(parser.pos(), (15, 4, 1));
    }

    #[test]
    fn test_parse_tag_attributes_whitespace() {
        let test_dir =
            Path::new(&env::var("CARGO_MANIFEST_DIR").unwrap()).join("src/htmlparser_tests");
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
                    Some(Token::Identifier((2, 1, 3), "a".to_string())),
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
            vec![HtmlParser::parse_identifier, HtmlParser::parse_number];
        let res = parser.try_parse(&parser_fns[..], "");
        assert_eq!(res, Ok(Token::Identifier((0, 1, 1), "abc".to_string())));
        assert_eq!(parser.pos(), (3, 1, 4));
    }

    #[test]
    fn test_try_parse2() {
        let mut parser = HtmlParser::new("123");
        let parser_fns: Vec<fn(&mut HtmlParser) -> ParserResult<Token>> =
            vec![HtmlParser::parse_identifier, HtmlParser::parse_number];
        let res = parser.try_parse(&parser_fns[..], "");
        assert_eq!(res, Ok(Token::Number((0, 1, 1), 123)));
        assert_eq!(parser.pos(), (3, 1, 4));
    }

    #[test]
    fn test_try_parse3() {
        let mut parser = HtmlParser::new("=");
        let parser_fns: Vec<fn(&mut HtmlParser) -> ParserResult<Token>> =
            vec![HtmlParser::parse_identifier, HtmlParser::parse_number];
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
            Ok(DomNode::new(NodeType::Html, vec![], (0, 1, 1), vec![]),)
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
                NodeType::Html,
                vec![
                    (
                        Token::Identifier((6, 1, 7), "a".to_string()),
                        Some(Token::Number((8, 1, 9), 1)),
                    ),
                ],
                (0, 1, 1),
                vec![]
            ),)
        );
        assert_eq!(parser.pos(), (10, 1, 11));
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
        assert_eq!(parser.pos(), (0, 1, 1));
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
        assert_eq!(parser.pos(), (0, 1, 1));
    }

    #[test]
    fn test_parse_node1() {
        let mut parser = HtmlParser::new("<html></html>");
        let res = parser.parse_node();
        assert_eq!(
            res,
            Ok(DomNode::new(NodeType::Html, vec![], (0, 1, 1), vec![]),)
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
        assert_eq!(parser.pos(), (0, 1, 1));
    }

    #[test]
    fn test_parse_text_node1() {
        let mut parser = HtmlParser::new("<html>hello</html>");
        let res = parser.parse_node();
        assert_eq!(
            res,
            Ok(DomNode::new(
                NodeType::Html,
                vec![],
                (0, 1, 1),
                vec![DomNode::new(NodeType::Text, vec![], (6, 1, 7), vec![])]
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
                NodeType::Html,
                vec![],
                (0, 1, 1),
                vec![DomNode::new(NodeType::Text, vec![], (7, 1, 8), vec![])]
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
                NodeType::Html,
                vec![],
                (0, 1, 1),
                vec![
                    DomNode::new(NodeType::Text, vec![], (7, 1, 8), vec![]),
                    DomNode::new(NodeType::Body, vec![], (19, 1, 20), vec![]),
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
                NodeType::Html,
                vec![],
                (0, 1, 1),
                vec![
                    DomNode::new(
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
                        (8, 1, 9),
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
                    DomNode::new(NodeType::Html, vec![], (0, 1, 1), vec![])
                )
            ))
        );
        assert_eq!(parser.pos(), (0, 1, 1));
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
        assert_eq!(parser.pos(), (0, 1, 1));
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
        assert_eq!(parser.pos(), (0, 1, 1));
    }
}
