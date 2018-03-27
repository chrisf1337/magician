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

type ParserResult<T> = Result<(T, Pos), Error>;
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
        Ok(((), self.pos()))
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
        Ok((
            Token::Identifier(start_pos, id.into_iter().collect()),
            self.pos(),
        ))
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
        let start_pos = self.pos();
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
            Ok((Token::Str(start_pos, st.into_iter().collect()), self.pos()))
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
        Ok((
            Token::Number(
                start_pos,
                id.into_iter().collect::<String>().parse::<i32>().unwrap(),
            ),
            self.pos(),
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
            Ok(((), self.pos()))
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
        let mut next_start_pos = self.pos();
        let mut attributes: Vec<(Token, Option<Token>)> = vec![];
        loop {
            match self.parse_identifier() {
                Ok((id, _)) => match self.parse_one_char('=') {
                    Ok(((), _)) => {
                        let parsers: Vec<ParserFn<Token>> = vec![
                            Self::parse_identifier,
                            Self::parse_string,
                            Self::parse_number,
                        ];
                        match self.try_parse(&parsers[..], "expected attribute value") {
                            Ok((val_token, new_pos)) => {
                                attributes.push((id, Some(val_token)));
                                next_start_pos = new_pos;
                            }
                            Err(_) => {
                                self.index = next_start_pos.0;
                                self.row = next_start_pos.1;
                                self.col = next_start_pos.2;
                                return Ok((attributes, self.pos()));
                            }
                        }
                    }
                    Err(_) => {
                        // no =, so the attribute doesn't have a value
                        attributes.push((id, None));
                    }
                },
                Err(_) => {
                    return Ok((attributes, self.pos()));
                }
            }
        }
    }

    fn parse_opening_tag(&mut self) -> ParserResult<DomNode> {
        let start_pos = self.pos();
        let _ = self.parse_one_char('<')?;
        let tag_start_pos = (self.index - 1, self.row, self.col - 1);
        let tag_id = match self.parse_identifier_strict() {
            Ok((tag_id, _)) => tag_id,
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
        let self_closing = match node_type {
            NodeType::Img => true,
            _ => false,
        };
        let (attrs, _) = self.parse_tag_attributes()?;
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
        Ok((
            DomNode::new(node_type, attrs, tag_start_pos, vec![]),
            self.pos(),
        ))
    }

    fn parse_closing_tag(&mut self, opening_tag: DomNode) -> ParserResult<DomNode> {
        let start_pos = self.pos();
        let _ = self.parse_one_char('<')?;
        let tag_start_pos = (self.index - 1, self.row, self.col - 1);
        let _ = self.parse_one_char_strict('/')?;
        let (tag_id, _) = self.parse_identifier_strict()?;
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
        Ok((opening_tag, self.pos()))
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
            Ok((
                DomNode::new(NodeType::Text, vec![], start_pos, vec![]),
                self.pos(),
            ))
        }
    }

    fn parse_node(&mut self) -> ParserResult<DomNode> {
        let start_pos = self.pos();
        let mut node = match self.parse_text_node() {
            Ok((node, _)) => node,
            Err(_) => match self.parse_opening_tag() {
                Ok((node, _)) => node,
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
                        Ok((child_node, _)) => child_node,
                        Err(err) => {
                            self.set_pos(start_pos);
                            return Err(err);
                        }
                    };
                    node.children.push(child_node);
                }
                _ => {
                    let text_node = match self.parse_text_node() {
                        Ok((text_node, _)) => text_node,
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
        let (node, _) = parser.parse_node()?;
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
    fn test_consume_whitespace1() {
        let mut parser = HtmlParser::new(" ab");
        let res = parser.consume_whitespace();
        assert_eq!(res, Ok(((), (1, 1, 2))));
        assert_eq!(parser.pos(), res.unwrap().1);
    }

    #[test]
    fn test_consume_whitespace2() {
        let mut parser = HtmlParser::new("ab");
        let res = parser.consume_whitespace();
        assert_eq!(res, Ok(((), (0, 1, 1))));
        assert_eq!(parser.pos(), res.unwrap().1);
    }

    #[test]
    fn test_parse_identifier1() {
        let mut parser = HtmlParser::new("asdf");
        let res = parser.parse_identifier();
        assert_eq!(
            res,
            Ok((Token::Identifier((0, 1, 1), "asdf".to_string()), (4, 1, 5)))
        );
        assert_eq!(parser.pos(), res.unwrap().1);
    }

    #[test]
    fn test_parse_identifier2() {
        let mut parser = HtmlParser::new("a1s2f");
        let res = parser.parse_identifier();
        assert_eq!(
            res,
            Ok((Token::Identifier((0, 1, 1), "a1s2f".to_string()), (5, 1, 6)))
        );
        assert_eq!(parser.pos(), res.unwrap().1);
    }

    #[test]
    fn test_parse_identifier_ignores_whitespace() {
        let mut parser = HtmlParser::new(" asdf");
        let res = parser.parse_identifier();
        assert_eq!(
            res,
            Ok((Token::Identifier((1, 1, 2), "asdf".to_string()), (5, 1, 6)))
        );
        assert_eq!(parser.pos(), res.unwrap().1);
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
        assert_eq!(
            res,
            Ok((Token::Str((0, 1, 1), "asdf".to_string()), (6, 1, 7)))
        );
        assert_eq!(parser.pos(), res.unwrap().1);
    }

    #[test]
    fn test_parse_string_single_quote() {
        let mut parser = HtmlParser::new("'asdf'");
        let res = parser.parse_string();
        assert_eq!(
            res,
            Ok((Token::Str((0, 1, 1), "asdf".to_string()), (6, 1, 7)))
        );
        assert_eq!(parser.pos(), res.unwrap().1);
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
        assert_eq!(res, Ok((Token::Number((0, 1, 1), 123), (3, 1, 4))));
        assert_eq!(parser.pos(), res.unwrap().1);
    }

    #[test]
    fn test_parse_number2() {
        let mut parser = HtmlParser::new("1b3");
        let res = parser.parse_number();
        assert_eq!(res, Ok((Token::Number((0, 1, 1), 1), (1, 1, 2))));
        assert_eq!(parser.pos(), res.unwrap().1);
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
            Ok((
                vec![
                    (
                        Token::Identifier((0, 1, 1), "a".to_string()),
                        Some(Token::Str((2, 1, 3), "a".to_string())),
                    ),
                    (
                        Token::Identifier((6, 1, 7), "b".to_string()),
                        Some(Token::Str((8, 1, 9), "b".to_string())),
                    ),
                ],
                (11, 1, 12)
            ))
        );
        assert_eq!(parser.pos(), res.unwrap().1);
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
            Ok((
                vec![
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
                ],
                (15, 4, 1)
            ))
        );
        assert_eq!(parser.pos(), res.unwrap().1);
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
            Ok((
                vec![
                    (
                        Token::Identifier((0, 1, 1), "a".to_string()),
                        Some(Token::Str((6, 1, 7), "a".to_string())),
                    ),
                    (
                        Token::Identifier((10, 2, 1), "b".to_string()),
                        Some(Token::Str((13, 3, 1), "b".to_string())),
                    ),
                ],
                (17, 4, 1)
            ))
        );
        assert_eq!(parser.pos(), res.unwrap().1);
    }

    #[test]
    fn test_parse_tag_attributes_early_termination1() {
        let mut parser = HtmlParser::new("a='a' <");
        let res = parser.parse_tag_attributes();
        assert_eq!(
            res,
            Ok((
                vec![
                    (
                        Token::Identifier((0, 1, 1), "a".to_string()),
                        Some(Token::Str((2, 1, 3), "a".to_string())),
                    ),
                ],
                (6, 1, 7)
            ))
        );
        assert_eq!(parser.pos(), res.unwrap().1);
    }

    #[test]
    fn test_parse_tag_attributes_early_termination2() {
        let mut parser = HtmlParser::new("a='a <");
        let res = parser.parse_tag_attributes();
        assert_eq!(res, Ok((vec![], (0, 1, 1))));
        assert_eq!(parser.pos(), res.unwrap().1);
    }

    #[test]
    fn test_parse_tag_attributes_none_val1() {
        let mut parser = HtmlParser::new("a=a b");
        let res = parser.parse_tag_attributes();
        assert_eq!(
            res,
            Ok((
                vec![
                    (
                        Token::Identifier((0, 1, 1), "a".to_string()),
                        Some(Token::Identifier((2, 1, 3), "a".to_string())),
                    ),
                    (Token::Identifier((4, 1, 5), "b".to_string()), None),
                ],
                (5, 1, 6)
            ))
        );
        assert_eq!(parser.pos(), res.unwrap().1);
    }

    #[test]
    fn test_parse_tag_attributes_none_val2() {
        let mut parser = HtmlParser::new("a b");
        let res = parser.parse_tag_attributes();
        assert_eq!(
            res,
            Ok((
                vec![
                    (Token::Identifier((0, 1, 1), "a".to_string()), None),
                    (Token::Identifier((2, 1, 3), "b".to_string()), None),
                ],
                (3, 1, 4)
            ))
        );
        assert_eq!(parser.pos(), res.unwrap().1);
    }

    #[test]
    fn test_parse_tag_attributes_none_val3() {
        let mut parser = HtmlParser::new("a");
        let res = parser.parse_tag_attributes();
        assert_eq!(
            res,
            Ok((
                vec![(Token::Identifier((0, 1, 1), "a".to_string()), None)],
                (1, 1, 2)
            ))
        );
        assert_eq!(parser.pos(), res.unwrap().1);
    }

    #[test]
    fn test_try_parse1() {
        let mut parser = HtmlParser::new("abc");
        let parser_fns: Vec<fn(&mut HtmlParser) -> ParserResult<Token>> =
            vec![HtmlParser::parse_identifier, HtmlParser::parse_number];
        let res = parser.try_parse(&parser_fns[..], "");
        assert_eq!(
            res,
            Ok((Token::Identifier((0, 1, 1), "abc".to_string()), (3, 1, 4)))
        );
        assert_eq!(parser.pos(), res.unwrap().1);
    }

    #[test]
    fn test_try_parse2() {
        let mut parser = HtmlParser::new("123");
        let parser_fns: Vec<fn(&mut HtmlParser) -> ParserResult<Token>> =
            vec![HtmlParser::parse_identifier, HtmlParser::parse_number];
        let res = parser.try_parse(&parser_fns[..], "");
        assert_eq!(res, Ok((Token::Number((0, 1, 1), 123), (3, 1, 4))));
        assert_eq!(parser.pos(), res.unwrap().1);
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
            Ok((
                DomNode::new(NodeType::Html, vec![], (0, 1, 1), vec![]),
                (6, 1, 7)
            ))
        );
        assert_eq!(parser.pos(), res.unwrap().1);
    }

    #[test]
    fn test_parse_opening_tag2() {
        let mut parser = HtmlParser::new("<html a=1>");
        let res = parser.parse_opening_tag();
        assert_eq!(
            res,
            Ok((
                DomNode::new(
                    NodeType::Html,
                    vec![
                        (
                            Token::Identifier((6, 1, 7), "a".to_string()),
                            Some(Token::Number((8, 1, 9), 1)),
                        ),
                    ],
                    (0, 1, 1),
                    vec![]
                ),
                (10, 1, 11)
            ))
        );
        assert_eq!(parser.pos(), res.unwrap().1);
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
            Ok((
                DomNode::new(NodeType::Html, vec![], (0, 1, 1), vec![]),
                (13, 1, 14)
            ))
        );
        assert_eq!(parser.pos(), res.unwrap().1);
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
            Ok((
                DomNode::new(
                    NodeType::Html,
                    vec![],
                    (0, 1, 1),
                    vec![DomNode::new(NodeType::Text, vec![], (6, 1, 7), vec![])]
                ),
                (18, 1, 19)
            ))
        );
        assert_eq!(parser.pos(), res.unwrap().1);
    }

    #[test]
    fn test_parse_text_node2() {
        let mut parser = HtmlParser::new("<html> hello    </html>");
        let res = parser.parse_node();
        assert_eq!(
            res,
            Ok((
                DomNode::new(
                    NodeType::Html,
                    vec![],
                    (0, 1, 1),
                    vec![DomNode::new(NodeType::Text, vec![], (7, 1, 8), vec![])]
                ),
                (23, 1, 24)
            ))
        );
        assert_eq!(parser.pos(), res.unwrap().1);
    }

    #[test]
    fn test_parse_multiple_nested_nodes() {
        let mut parser = HtmlParser::new("<html> hello hello <body></body>  </html>");
        let res = parser.parse_node();
        assert_eq!(
            res,
            Ok((
                DomNode::new(
                    NodeType::Html,
                    vec![],
                    (0, 1, 1),
                    vec![
                        DomNode::new(NodeType::Text, vec![], (7, 1, 8), vec![]),
                        DomNode::new(NodeType::Body, vec![], (19, 1, 20), vec![]),
                    ]
                ),
                (41, 1, 42)
            ))
        );
        assert_eq!(parser.pos(), res.unwrap().1);
    }

    #[test]
    fn test_parse_multiple_nested_nodes_attrs() {
        let mut parser = HtmlParser::new("<html>  <body a='b'  c=\"d\"></body>  </html>");
        let res = parser.parse_node();
        assert_eq!(
            res,
            Ok((
                DomNode::new(
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
                ),
                (43, 1, 44)
            ))
        );
        assert_eq!(parser.pos(), res.unwrap().1);
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
    fn test_parse_fail_mismatched_closing_tag() {
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
}
