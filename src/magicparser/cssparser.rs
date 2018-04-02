use magicparser::common::Pos;
use magicparser::error::{Error, Result};
use magicparser::lexer::Lexer;
use magicparser::parser::Parser;
use magicparser::selectorparser::{Selector, SelectorParser};

type DeclBlock = Vec<(Token, Token)>;
type IntermediateBlock = (Token, DeclBlock);
type Block = (Selector, DeclBlock);

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

    fn parse_selector(&mut self) -> Result<Token> {
        self.lexer.consume_whitespace()?;
        let start_pos = self.pos();
        let mut selector: Vec<char> = vec![];
        loop {
            match self.lexer.peek_char() {
                Ok((_, ch)) => if ch != '{' && ch != '\n' && ch != ';' {
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

    fn parse_property(&mut self) -> Result<Token> {
        let _ = self.lexer.consume_whitespace()?;
        let start_pos = self.pos();
        let mut property: Vec<char> = vec![];
        match self.lexer.peek_char() {
            Ok((_, ch)) => if ch.is_ascii_alphabetic() {
                property.push(ch);
                self.lexer.consume_char()?;
            } else {
                return Err(Error::Unexpected(
                    start_pos,
                    "expected property".to_string(),
                ));
            },
            Err(err) => return Err(err),
        }
        loop {
            match self.lexer.peek_char() {
                Ok((_, ch)) => if ch.is_ascii_alphanumeric() || ch == '-' {
                    property.push(ch);
                    self.lexer.consume_char()?;
                } else {
                    break;
                },
                Err(_) => break,
            }
        }
        if property.is_empty() {
            Err(Error::Unexpected(
                start_pos,
                "expected property".to_string(),
            ))
        } else {
            Ok(Token::Property(start_pos, property.into_iter().collect()))
        }
    }

    fn parse_value(&mut self) -> Result<Token> {
        let _ = self.lexer.consume_whitespace()?;
        let start_pos = self.pos();
        let mut value: Vec<char> = vec![];
        loop {
            match self.lexer.peek_char() {
                Ok((_, ch)) => if ch != ';' && ch != '}' {
                    value.push(ch);
                    self.lexer.consume_char()?;
                } else {
                    break;
                },
                Err(_) => break,
            }
        }
        if value.is_empty() {
            Err(Error::Unexpected(start_pos, "expected value".to_string()))
        } else {
            Ok(Token::Value(
                start_pos,
                value.into_iter().collect::<String>().trim().to_string(),
            ))
        }
    }

    fn parse_decl_block(&mut self) -> Result<DeclBlock> {
        let mut declarations: Vec<(Token, Token)> = vec![];
        let block_start = self.lexer.parse_chars("{")?;
        loop {
            let property = match self.parse_property() {
                Ok(p) => p,
                Err(_) => break,
            };
            self.lexer.try_parse_chars(":")?;
            let value = self.parse_value()?;
            declarations.push((property, value));
            match self.lexer.try_parse_chars(";") {
                Ok(_) => (),
                Err(_) => break,
            }
        }
        match self.lexer.parse_chars("}") {
            Ok(_) => (),
            Err(_) => {
                return Err(Error::Unexpected(block_start, "unclosed block".to_string()));
            }
        }
        Ok(declarations)
    }

    fn parse_block(&mut self) -> Result<IntermediateBlock> {
        let selector = self.parse_selector()?;
        let decl_block = self.parse_decl_block()?;
        Ok((selector, decl_block))
    }

    fn parse_blocks(&mut self) -> Result<Vec<IntermediateBlock>> {
        let mut blocks = vec![];
        loop {
            match self.parse_block() {
                Ok(bl) => blocks.push(bl),
                Err(_) => break,
            }
        }
        Ok(blocks)
    }

    pub fn parse(input: &str) -> Result<Vec<Block>> {
        let mut parser = CssParser::new(input);
        let int_blocks = parser.parse_blocks()?;
        let mut blocks = vec![];
        for (token, decl_block) in int_blocks.into_iter() {
            match token {
                Token::Selector(pos, sel_str) => {
                    blocks.push((SelectorParser::parse(&sel_str, pos)?, decl_block))
                }

                _ => unreachable!(),
            }
        }
        Ok(blocks)
    }
}

impl Parser<Error> for CssParser {
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
    use magicparser::common::ElemType;
    use magicparser::selectorparser::*;
    use std::env;
    use std::fs::File;
    use std::io::prelude::*;
    use std::path::Path;

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

    #[test]
    fn test_parse_decl_block1() {
        let mut parser = CssParser::new("{ aa: bb; c: d; }");
        let res = parser.parse_decl_block();
        assert_eq!(
            res,
            Ok(vec![
                (
                    Token::Property((2, 1, 3), "aa".to_string()),
                    Token::Value((6, 1, 7), "bb".to_string()),
                ),
                (
                    Token::Property((10, 1, 11), "c".to_string()),
                    Token::Value((13, 1, 14), "d".to_string()),
                ),
            ])
        );
        assert_eq!(parser.pos(), (17, 1, 18));
    }

    #[test]
    fn test_parse_decl_block2() {
        let mut parser = CssParser::new("{ aa: bb !important ; c: d }");
        let res = parser.parse_decl_block();
        assert_eq!(
            res,
            Ok(vec![
                (
                    Token::Property((2, 1, 3), "aa".to_string()),
                    Token::Value((6, 1, 7), "bb !important".to_string()),
                ),
                (
                    Token::Property((22, 1, 23), "c".to_string()),
                    Token::Value((25, 1, 26), "d".to_string()),
                ),
            ])
        );
        assert_eq!(parser.pos(), (28, 1, 29));
    }

    #[test]
    fn test_parse_decl_block_no_terminating_semicolon1() {
        let mut parser = CssParser::new("{ aa: bb; c: d }");
        let res = parser.parse_decl_block();
        assert_eq!(
            res,
            Ok(vec![
                (
                    Token::Property((2, 1, 3), "aa".to_string()),
                    Token::Value((6, 1, 7), "bb".to_string()),
                ),
                (
                    Token::Property((10, 1, 11), "c".to_string()),
                    Token::Value((13, 1, 14), "d".to_string()),
                ),
            ])
        );
        assert_eq!(parser.pos(), (16, 1, 17));
    }

    #[test]
    fn test_parse_decl_block_no_terminating_semicolon2() {
        let mut parser = CssParser::new("{ aa: bb}");
        let res = parser.parse_decl_block();
        assert_eq!(
            res,
            Ok(vec![
                (
                    Token::Property((2, 1, 3), "aa".to_string()),
                    Token::Value((6, 1, 7), "bb".to_string()),
                ),
            ])
        );
        assert_eq!(parser.pos(), (9, 1, 10));
    }

    #[test]
    fn test_parse_decl_block_value_is_trimmed() {
        let mut parser = CssParser::new("{ aa: bb  \n }");
        let res = parser.parse_decl_block();
        assert_eq!(
            res,
            Ok(vec![
                (
                    Token::Property((2, 1, 3), "aa".to_string()),
                    Token::Value((6, 1, 7), "bb".to_string()),
                ),
            ])
        );
        assert_eq!(parser.pos(), (13, 2, 3));
    }

    #[test]
    fn test_parse_decl_block_unclosed_block1() {
        let mut parser = CssParser::new("{ aa: bb; c: d; ");
        let res = parser.parse_decl_block();
        assert_eq!(
            res,
            Err(Error::Unexpected((0, 1, 1), "unclosed block".to_string()))
        );
        assert_eq!(parser.pos(), (16, 1, 17));
    }

    #[test]
    fn test_parse_decl_block_unclosed_block2() {
        let mut parser = CssParser::new("{ aa: bb; c: d  ");
        let res = parser.parse_decl_block();
        assert_eq!(
            res,
            Err(Error::Unexpected((0, 1, 1), "unclosed block".to_string()))
        );
        assert_eq!(parser.pos(), (16, 1, 17));
    }

    #[test]
    fn test_parse_block() {
        let mut parser = CssParser::new("a { font-size: 2em; }");
        let res = parser.parse_block();
        assert_eq!(
            res,
            Ok((
                Token::Selector((0, 1, 1), "a".to_string()),
                vec![
                    (
                        Token::Property((4, 1, 5), "font-size".to_string()),
                        Token::Value((15, 1, 16), "2em".to_string()),
                    ),
                ]
            ))
        );
        assert_eq!(parser.pos(), (21, 1, 22));
    }

    #[test]
    fn test_parse_block_fail() {
        let mut parser = CssParser::new("a font-size: 2em; }");
        let res = parser.parse_block();
        assert_eq!(
            res,
            Err(Error::Unexpected(
                (16, 1, 17),
                "expected '{', got ';'".to_string()
            ))
        );
        assert_eq!(parser.pos(), (16, 1, 17));
    }

    #[test]
    fn test_parse_blocks() {
        let mut parser = CssParser::new("a {} b {}");
        let res = parser.parse_blocks();
        assert_eq!(
            res,
            Ok(vec![
                (Token::Selector((0, 1, 1), "a".to_string()), vec![]),
                (Token::Selector((5, 1, 6), "b".to_string()), vec![]),
            ])
        );
        assert_eq!(parser.pos(), (9, 1, 10));
    }

    #[test]
    fn test_simple1() {
        let test_dir = Path::new(&env::var("CARGO_MANIFEST_DIR").unwrap_or(DEFAULT_CARGO_MANIFEST_DIR.to_string()))
            .join("src/magicparser/cssparser_tests");
        let mut f = File::open(test_dir.join("simple.css")).expect("file not found");
        let mut input = String::new();
        f.read_to_string(&mut input).expect("read");
        let mut parser = CssParser::new(&input);
        let res = parser.parse_blocks();
        assert_eq!(
            res,
            Ok(vec![
                (
                    Token::Selector((0, 1, 1), "a:link, a:visited".to_string()),
                    vec![
                        (
                            Token::Property((22, 2, 3), "background-color".to_string()),
                            Token::Value((40, 2, 21), "#f44336".to_string()),
                        ),
                        (
                            Token::Property((51, 3, 3), "color".to_string()),
                            Token::Value((58, 3, 10), "white".to_string()),
                        ),
                        (
                            Token::Property((67, 4, 3), "padding".to_string()),
                            Token::Value((76, 4, 12), "14px 25px".to_string()),
                        ),
                    ],
                ),
                (
                    Token::Selector((90, 7, 1), "a:hover, a:active".to_string()),
                    vec![
                        (
                            Token::Property((112, 8, 3), "background-color".to_string()),
                            Token::Value((130, 8, 21), "red".to_string()),
                        ),
                    ],
                ),
            ])
        );
    }

    #[test]
    fn test_simple2() {
        let test_dir = Path::new(&env::var("CARGO_MANIFEST_DIR").unwrap_or(DEFAULT_CARGO_MANIFEST_DIR.to_string()))
            .join("src/magicparser/cssparser_tests");
        let mut f = File::open(test_dir.join("simple.css")).expect("file not found");
        let mut input = String::new();
        f.read_to_string(&mut input).expect("read");
        let res = CssParser::parse(&input);
        assert_eq!(
            res,
            Ok(vec![
                (
                    Selector::Group(vec![
                        Selector::Seq(vec![
                            Selector::Simple(SimpleSelector::new(
                                (0, 1, 1),
                                Some(ElemType::A),
                                None,
                                vec![],
                                false,
                            )),
                            Selector::PseudoClass(PseudoClassSelector::new(
                                (1, 1, 2),
                                PseudoClassSelectorType::Link,
                            )),
                        ]),
                        Selector::Seq(vec![
                            Selector::Simple(SimpleSelector::new(
                                (8, 1, 9),
                                Some(ElemType::A),
                                None,
                                vec![],
                                false,
                            )),
                            Selector::PseudoClass(PseudoClassSelector::new(
                                (9, 1, 10),
                                PseudoClassSelectorType::Visited,
                            )),
                        ]),
                    ]),
                    vec![
                        (
                            Token::Property((22, 2, 3), "background-color".to_string()),
                            Token::Value((40, 2, 21), "#f44336".to_string()),
                        ),
                        (
                            Token::Property((51, 3, 3), "color".to_string()),
                            Token::Value((58, 3, 10), "white".to_string()),
                        ),
                        (
                            Token::Property((67, 4, 3), "padding".to_string()),
                            Token::Value((76, 4, 12), "14px 25px".to_string()),
                        ),
                    ],
                ),
                (
                    Selector::Group(vec![
                        Selector::Seq(vec![
                            Selector::Simple(SimpleSelector::new(
                                (90, 7, 1),
                                Some(ElemType::A),
                                None,
                                vec![],
                                false,
                            )),
                            Selector::PseudoClass(PseudoClassSelector::new(
                                (91, 7, 2),
                                PseudoClassSelectorType::Hover,
                            )),
                        ]),
                        Selector::Seq(vec![
                            Selector::Simple(SimpleSelector::new(
                                (99, 7, 10),
                                Some(ElemType::A),
                                None,
                                vec![],
                                false,
                            )),
                            Selector::PseudoClass(PseudoClassSelector::new(
                                (100, 7, 11),
                                PseudoClassSelectorType::Active,
                            )),
                        ]),
                    ]),
                    vec![
                        (
                            Token::Property((112, 8, 3), "background-color".to_string()),
                            Token::Value((130, 8, 21), "red".to_string()),
                        ),
                    ],
                ),
            ])
        );
    }
}
