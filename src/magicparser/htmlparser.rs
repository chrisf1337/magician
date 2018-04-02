use magicparser::common::{ElemType, Pos, Token};
use magicparser::error::{Error, Result};
use magicparser::lexer::Lexer;
use magicparser::parser::Parser;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct DomNode {
    pub pos: Pos,
    pub elem_type: ElemType,
    pub attrs: Vec<(Token, Option<Token>)>, // (AttrIdentifier, Value) or (AttrIdentifier, Str)
    pub children: Vec<DomNode>,
}

impl DomNode {
    fn new(
        pos: Pos,
        elem_type: ElemType,
        attrs: Vec<(Token, Option<Token>)>,
        children: Vec<DomNode>,
    ) -> DomNode {
        DomNode {
            pos,
            elem_type,
            attrs,
            children,
        }
    }
}

pub struct HtmlParser {
    lexer: Lexer,
}

type ParserFn<T> = fn(&mut HtmlParser) -> Result<T>;

impl HtmlParser {
    fn new(input: &str) -> HtmlParser {
        HtmlParser {
            lexer: Lexer::new(input, "<!--", "-->"),
        }
    }

    fn parse_value(&mut self) -> Result<Token> {
        let _ = self.lexer.consume_whitespace()?;
        let start_pos = self.pos();
        let mut id: Vec<char> = vec![];
        match self.lexer.peek_char() {
            Ok((_, ch)) => if ch != '<' && ch != '>' && ch != '"' && ch != '\'' {
                id.push(ch);
                self.lexer.consume_char()?;
            } else {
                return Err(Error::Unexpected(start_pos, "expected value".to_string()));
            },
            Err(err) => return Err(err),
        }
        loop {
            match self.lexer.peek_char() {
                Ok((_, ch)) => if !ch.is_ascii_whitespace() && ch != '<' && ch != '>' && ch != '"'
                    && ch != '\''
                {
                    id.push(ch);
                    self.lexer.consume_char()?;
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

    fn parse_tag_attributes(&mut self) -> Result<Vec<(Token, Option<Token>)>> {
        // roll back to next_start_pos if we parse an incomplete pair
        let mut next_start_pos = self.pos();
        let mut attributes: Vec<(Token, Option<Token>)> = vec![];
        loop {
            match self.parse_attr_identifier() {
                Ok(id) => match self.lexer.try_parse_one_char('=') {
                    Ok(_) => {
                        let parsers: Vec<ParserFn<Token>> =
                            vec![Self::parse_string, Self::parse_value];
                        match self.try_parsers(&parsers[..], "expected attribute value") {
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

    fn parse_doctype(&mut self) -> Result<()> {
        match self.lexer.parse_chars("<!DOCTYPE html>") {
            Ok(_) => Ok(()),
            Err(err) => Err(err),
        }
    }

    fn parse_opening_tag(&mut self) -> Result<DomNode> {
        let tag_start_pos = self.lexer.try_parse_one_char('<')?;
        let tag_id = match self.parse_elem_identifier_strict() {
            Ok(tag_id) => tag_id,
            Err(err) => {
                return Err(err);
            }
        };
        let elem_type = match tag_id {
            Token::ElemIdentifier(_, tag_id_str) => ElemType::from(&tag_id_str),
            _ => unreachable!(),
        };
        if elem_type.is_void_elem() {
            let attrs = self.parse_tag_attributes()?;
            match self.lexer.try_parse_chars("/>") {
                Ok(_) => (),
                Err(_) => match self.lexer.try_parse_chars(">") {
                    Ok(_) => (),
                    Err(_) => {
                        return Err(Error::Unexpected(
                            tag_start_pos,
                            format!("unclosed element: {:?}", elem_type),
                        ))
                    }
                },
            };
            Ok(DomNode::new(tag_start_pos, elem_type, attrs, vec![]))
        } else {
            let attrs = self.parse_tag_attributes()?;
            match self.lexer.try_parse_one_char('>') {
                Ok(_) => (),
                Err(_) => {
                    return Err(Error::Unexpected(
                        tag_start_pos,
                        format!("unclosed tag: {:?}", elem_type),
                    ));
                }
            };
            Ok(DomNode::new(tag_start_pos, elem_type, attrs, vec![]))
        }
    }

    fn parse_closing_tag(&mut self, opening_tag: DomNode) -> Result<DomNode> {
        let tag_start_pos = self.lexer.try_parse_one_char('<')?;
        let _ = self.lexer.try_parse_one_char_strict('/')?;
        let tag_id = self.parse_elem_identifier_strict()?;
        let elem_type = match tag_id {
            Token::ElemIdentifier(_, tag_id_str) => ElemType::from(&tag_id_str),
            _ => unreachable!(),
        };
        if opening_tag.elem_type != elem_type {
            return Err(Error::Unexpected(
                tag_start_pos,
                format!(
                    "expected closing tag for {:?}, got {:?}",
                    opening_tag.elem_type, elem_type
                ),
            ));
        }
        let _ = self.lexer.try_parse_one_char('>')?;
        Ok(opening_tag)
    }

    fn parse_text_node(&mut self) -> Result<DomNode> {
        let start_pos = self.pos();
        let mut text: Vec<char> = vec![];
        loop {
            match self.lexer.peek_char() {
                Ok((_, ch)) => if ch != '<' {
                    let (_, ch) = self.lexer.consume_char()?;
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
            Ok(DomNode::new(start_pos, ElemType::Text(s), vec![], vec![]))
        }
    }

    fn parse_node(&mut self) -> Result<DomNode> {
        let mut node = match self.try(HtmlParser::parse_text_node) {
            Ok(node) => node,
            Err(_) => match self.try(HtmlParser::parse_opening_tag) {
                Ok(node) => node,
                Err(err) => {
                    return Err(err);
                }
            },
        };
        if node.elem_type.is_void_elem() {
            return Ok(node);
        }
        loop {
            match self.lexer.consume_whitespace() {
                Ok(_) => (),
                Err(Error::Eof(_)) => {
                    return Err(Error::Unexpected(
                        node.pos,
                        format!("unclosed element: {:?}", node),
                    ));
                }
                Err(e) => return Err(e),
            };
            if self.lexer.eof() {
                return Err(Error::Unexpected(
                    node.pos,
                    format!("unclosed element: {:?}", node),
                ));
            }
            match self.lexer.peek_chars(2) {
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

    pub fn parse(input: &str) -> Result<DomNode> {
        let mut parser = HtmlParser::new(input);
        let _ = parser.try(HtmlParser::parse_doctype);
        let node = parser.parse_node()?;
        Ok(node)
    }
}

impl Parser<Error> for HtmlParser {
    fn lexer(&mut self) -> &mut Lexer {
        &mut self.lexer
    }

    fn lexer_immut(&self) -> &Lexer {
        &self.lexer
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use magicparser::common::DEFAULT_CARGO_MANIFEST_DIR;
    use std::env;
    use std::fs::File;
    use std::io::prelude::*;
    use std::path::Path;

    #[test]
    fn test_parse_tag_attributes1() {
        let mut parser = HtmlParser::new("a=\"a\" b=\"b\"");
        let res = parser.parse_tag_attributes();
        assert_eq!(
            res,
            Ok(vec![
                (
                    Token::AttrIdentifier((0, 1, 1), "a".to_string()),
                    Some(Token::Str((2, 1, 3), "a".to_string())),
                ),
                (
                    Token::AttrIdentifier((6, 1, 7), "b".to_string()),
                    Some(Token::Str((8, 1, 9), "b".to_string())),
                ),
            ],)
        );
        assert_eq!(parser.pos(), (11, 1, 12));
    }

    #[test]
    fn test_parse_tag_attributes_multiple_value_types() {
        let test_dir = Path::new(&env::var("CARGO_MANIFEST_DIR").unwrap_or(DEFAULT_CARGO_MANIFEST_DIR.to_string()))
            .join("src/magicparser/htmlparser_tests");
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
                    Token::AttrIdentifier((0, 1, 1), "a".to_string()),
                    Some(Token::Str((2, 1, 3), "1".to_string())),
                ),
                (
                    Token::AttrIdentifier((6, 2, 1), "b".to_string()),
                    Some(Token::Value((8, 2, 3), "1".to_string())),
                ),
                (
                    Token::AttrIdentifier((10, 3, 1), "c".to_string()),
                    Some(Token::Value((12, 3, 3), "a1".to_string())),
                ),
            ],)
        );
        assert_eq!(parser.pos(), (15, 4, 1));
    }

    #[test]
    fn test_parse_tag_attributes_whitespace() {
        let test_dir = Path::new(&env::var("CARGO_MANIFEST_DIR").unwrap_or(DEFAULT_CARGO_MANIFEST_DIR.to_string()))
            .join("src/magicparser/htmlparser_tests");
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
                    Token::AttrIdentifier((0, 1, 1), "a".to_string()),
                    Some(Token::Str((6, 1, 7), "a".to_string())),
                ),
                (
                    Token::AttrIdentifier((10, 2, 1), "b".to_string()),
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
                    Token::AttrIdentifier((0, 1, 1), "a".to_string()),
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
            Ok(vec![
                (Token::AttrIdentifier((0, 1, 1), "a".to_string()), None),
            ])
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
                    Token::AttrIdentifier((0, 1, 1), "a".to_string()),
                    Some(Token::Value((2, 1, 3), "a".to_string())),
                ),
                (Token::AttrIdentifier((4, 1, 5), "b".to_string()), None),
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
                (Token::AttrIdentifier((0, 1, 1), "a".to_string()), None),
                (Token::AttrIdentifier((2, 1, 3), "b".to_string()), None),
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
            Ok(vec![
                (Token::AttrIdentifier((0, 1, 1), "a".to_string()), None),
            ],)
        );
        assert_eq!(parser.pos(), (1, 1, 2));
    }

    #[test]
    fn test_try_parse1() {
        let mut parser = HtmlParser::new("abc");
        let parser_fns: Vec<fn(&mut HtmlParser) -> Result<Token>> =
            vec![HtmlParser::parse_attr_identifier, HtmlParser::parse_value];
        let res = parser.try_parsers(&parser_fns[..], "");
        assert_eq!(res, Ok(Token::AttrIdentifier((0, 1, 1), "abc".to_string())));
        assert_eq!(parser.pos(), (3, 1, 4));
    }

    #[test]
    fn test_try_parse2() {
        let mut parser = HtmlParser::new("123");
        let parser_fns: Vec<fn(&mut HtmlParser) -> Result<Token>> =
            vec![HtmlParser::parse_attr_identifier, HtmlParser::parse_value];
        let res = parser.try_parsers(&parser_fns[..], "");
        assert_eq!(res, Ok(Token::Value((0, 1, 1), "123".to_string())));
        assert_eq!(parser.pos(), (3, 1, 4));
    }

    #[test]
    fn test_try_parse3() {
        let mut parser = HtmlParser::new("<");
        let parser_fns: Vec<fn(&mut HtmlParser) -> Result<Token>> =
            vec![HtmlParser::parse_attr_identifier, HtmlParser::parse_value];
        let res = parser.try_parsers(&parser_fns[..], "error message");
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
            Ok(DomNode::new((0, 1, 1), ElemType::Html, vec![], vec![]),)
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
                ElemType::Html,
                vec![
                    (
                        Token::AttrIdentifier((6, 1, 7), "a".to_string()),
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
                ElemType::Html,
                vec![
                    (
                        Token::AttrIdentifier((6, 1, 7), "a-1".to_string()),
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
                format!("unclosed tag: {:?}", ElemType::Html)
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
                "expected element identifier".to_string()
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
                format!("unclosed tag: {:?}", ElemType::Body)
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
            Ok(DomNode::new((0, 1, 1), ElemType::Html, vec![], vec![]),)
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
                "expected element identifier".to_string()
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
                    DomNode::new((0, 1, 1), ElemType::Html, vec![], vec![])
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
                    DomNode::new((0, 1, 1), ElemType::Html, vec![], vec![])
                )
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
                ElemType::Html,
                vec![],
                vec![
                    DomNode::new(
                        (6, 1, 7),
                        ElemType::Text("hello".to_string()),
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
                ElemType::Html,
                vec![],
                vec![
                    DomNode::new(
                        (7, 1, 8),
                        ElemType::Text("hello".to_string()),
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
                ElemType::Html,
                vec![],
                vec![
                    DomNode::new(
                        (7, 1, 8),
                        ElemType::Text("hello hello".to_string()),
                        vec![],
                        vec![],
                    ),
                    DomNode::new((19, 1, 20), ElemType::Body, vec![], vec![]),
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
                ElemType::Html,
                vec![],
                vec![
                    DomNode::new(
                        (8, 1, 9),
                        ElemType::Body,
                        vec![
                            (
                                Token::AttrIdentifier((14, 1, 15), "a".to_string()),
                                Some(Token::Str((16, 1, 17), "b".to_string())),
                            ),
                            (
                                Token::AttrIdentifier((21, 1, 22), "c".to_string()),
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
    fn test_parse_node_case() {
        let mut parser = HtmlParser::new("<div id=An-Id></div>");
        let res = parser.parse_node();
        assert_eq!(
            res,
            Ok(DomNode::new(
                (0, 1, 1),
                ElemType::Div,
                vec![
                    (
                        Token::AttrIdentifier((5, 1, 6), "id".to_string()),
                        Some(Token::Value((8, 1, 9), "An-Id".to_string())),
                    ),
                ],
                vec![]
            ))
        );
        assert_eq!(parser.pos(), (20, 1, 21));
    }

    #[test]
    fn test_parse_node_fail_eof1() {
        let mut parser = HtmlParser::new("<html");
        let res = parser.parse_node();
        assert_eq!(
            res,
            Err(Error::Unexpected(
                (0, 1, 1),
                format!("unclosed tag: {:?}", ElemType::Html)
            ))
        );
        assert_eq!(parser.pos(), (0, 1, 1));
    }

    #[test]
    fn test_parse_node_fail_eof2() {
        let mut parser = HtmlParser::new("<html>");
        let res = parser.parse_node();
        assert_eq!(
            res,
            Err(Error::Unexpected(
                (0, 1, 1),
                format!(
                    "unclosed element: {:?}",
                    DomNode::new((0, 1, 1), ElemType::Html, vec![], vec![])
                )
            ))
        );
        assert_eq!(parser.pos(), (6, 1, 7));
    }

    #[test]
    fn test_parse_node_fail_mismatched_closing_tag1() {
        let mut parser = HtmlParser::new("<html></body>");
        let res = parser.parse_node();
        assert_eq!(
            res,
            Err(Error::Unexpected(
                (6, 1, 7),
                format!(
                    "expected closing tag for {:?}, got {:?}",
                    ElemType::Html,
                    ElemType::Body
                )
            ))
        );
        // terminates at 12 since we check for paired tags as soon as the
        // identifier for the closing tag has been parsed
        assert_eq!(parser.pos(), (12, 1, 13));
    }

    #[test]
    fn test_parse_node_fail_mismatched_closing_tag2() {
        let mut parser = HtmlParser::new("<html><body></body><body></body></body>");
        let res = parser.parse_node();
        assert_eq!(
            res,
            Err(Error::Unexpected(
                (32, 1, 33),
                format!(
                    "expected closing tag for {:?}, got {:?}",
                    ElemType::Html,
                    ElemType::Body
                )
            ))
        );
        assert_eq!(parser.pos(), (38, 1, 39));
    }

    #[test]
    fn test_parse_node_with_comment1() {
        let mut parser = HtmlParser::new("<ht<!-- -->ml></html>  ");
        let res = parser.parse_node();
        assert_eq!(
            res,
            Ok(DomNode::new((0, 1, 1), ElemType::Html, vec![], vec![]))
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
                ElemType::Img,
                vec![
                    (
                        Token::AttrIdentifier((5, 1, 6), "src".to_string()),
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
                ElemType::Img,
                vec![
                    (
                        Token::AttrIdentifier((5, 1, 6), "src".to_string()),
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
                "unclosed element: Img".to_string()
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
                ElemType::Img,
                vec![
                    (
                        Token::AttrIdentifier((20, 1, 21), "src".to_string()),
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
        let test_dir = Path::new(&env::var("CARGO_MANIFEST_DIR").unwrap_or(DEFAULT_CARGO_MANIFEST_DIR.to_string()))
            .join("src/magicparser/htmlparser_tests");
        let mut f = File::open(test_dir.join("simple.html")).expect("file not found");
        let mut input = String::new();
        f.read_to_string(&mut input).expect("read");
        let res = HtmlParser::parse(&input);
        assert_eq!(
            res,
            Ok(DomNode::new(
                (16, 2, 1),
                ElemType::Html,
                vec![],
                vec![
                    DomNode::new(
                        (23, 3, 1),
                        ElemType::Body,
                        vec![],
                        vec![
                            DomNode::new(
                                (31, 5, 1),
                                ElemType::H1,
                                vec![],
                                vec![
                                    DomNode::new(
                                        (35, 5, 5),
                                        ElemType::Text("My First Heading".to_string()),
                                        vec![],
                                        vec![],
                                    ),
                                ],
                            ),
                            DomNode::new(
                                (57, 6, 1),
                                ElemType::A,
                                vec![
                                    (
                                        Token::AttrIdentifier((60, 6, 4), "href".to_string()),
                                        Some(Token::Value(
                                            (65, 6, 9),
                                            "https://www.google.com".to_string(),
                                        )),
                                    ),
                                ],
                                vec![
                                    DomNode::new(
                                        (88, 6, 32),
                                        ElemType::Text("Link".to_string()),
                                        vec![],
                                        vec![],
                                    ),
                                ],
                            ),
                            DomNode::new(
                                (97, 7, 1),
                                ElemType::P,
                                vec![],
                                vec![
                                    DomNode::new(
                                        (100, 7, 4),
                                        ElemType::Text("My first paragraph.".to_string()),
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
