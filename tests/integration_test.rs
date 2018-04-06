#[cfg(test)]
#[macro_use]
extern crate maplit;
#[cfg(test)]
#[macro_use]
extern crate pretty_assertions;

extern crate magician;
use magician::magicparser::{parse_css, parse_html, CssBlocks, DomNode, ElemType,
                            PseudoClassSelector, Selector, SimpleSelector,
                            DEFAULT_CARGO_MANIFEST_DIR};

use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

#[test]
fn test_htmlparser() {
    let test_dir = Path::new(&env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| DEFAULT_CARGO_MANIFEST_DIR.to_string()))
        .join("src/magicparser/htmlparser_tests");
    let mut f = File::open(test_dir.join("simple.html")).expect("file not found");
    let mut input = String::new();
    f.read_to_string(&mut input).expect("read");
    assert_eq!(
        parse_html(&input),
        Ok(DomNode::new(
            ElemType::Html,
            None,
            hashset!{},
            hashmap!{},
            vec![
                DomNode::new(
                    ElemType::Body,
                    None,
                    hashset!{},
                    hashmap!{},
                    vec![
                        DomNode::new(
                            ElemType::H1,
                            None,
                            hashset!{},
                            hashmap!{},
                            vec![
                                DomNode::new(
                                    ElemType::Text("My First Heading".to_string()),
                                    None,
                                    hashset!{},
                                    hashmap![],
                                    vec![],
                                ),
                            ],
                        ),
                        DomNode::new(
                            ElemType::A,
                            None,
                            hashset!{},
                            hashmap! {
                                "href".to_string() => Some("https://www.google.com".to_string())
                            },
                            vec![
                                DomNode::new(
                                    ElemType::Text("Link".to_string()),
                                    None,
                                    hashset!{},
                                    hashmap!{},
                                    vec![],
                                ),
                            ],
                        ),
                        DomNode::new(
                            ElemType::P,
                            None,
                            hashset!{},
                            hashmap!{},
                            vec![
                                DomNode::new(
                                    ElemType::Text("My first paragraph.".to_string()),
                                    None,
                                    hashset!{},
                                    hashmap!{},
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

#[test]
fn test_cssparser() {
    let test_dir = Path::new(&env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| DEFAULT_CARGO_MANIFEST_DIR.to_string()))
        .join("src/magicparser/cssparser_tests");
    let mut f = File::open(test_dir.join("simple.css")).expect("file not found");
    let mut input = String::new();
    f.read_to_string(&mut input).expect("read");
    assert_eq!(
        parse_css(&input),
        Ok(CssBlocks(vec![
            (
                Selector::Group(vec![
                    Selector::Seq(vec![
                        Selector::Simple(SimpleSelector::new(
                            Some(ElemType::A),
                            None,
                            hashset!{},
                            false,
                        )),
                        Selector::PseudoClass(PseudoClassSelector::Link),
                    ]),
                    Selector::Seq(vec![
                        Selector::Simple(SimpleSelector::new(
                            Some(ElemType::A),
                            None,
                            hashset!{},
                            false,
                        )),
                        Selector::PseudoClass(PseudoClassSelector::Visited),
                    ]),
                ]),
                hashmap! {
                    "background-color".to_string() => "#f44336".to_string(),
                    "color".to_string() => "white".to_string(),
                    "padding".to_string() => "14px 25px".to_string(),
                },
            ),
            (
                Selector::Group(vec![
                    Selector::Seq(vec![
                        Selector::Simple(SimpleSelector::new(
                            Some(ElemType::A),
                            None,
                            hashset!{},
                            false,
                        )),
                        Selector::PseudoClass(PseudoClassSelector::Hover),
                    ]),
                    Selector::Seq(vec![
                        Selector::Simple(SimpleSelector::new(
                            Some(ElemType::A),
                            None,
                            hashset!{},
                            false,
                        )),
                        Selector::PseudoClass(PseudoClassSelector::Active),
                    ]),
                ]),
                hashmap! {
                    "background-color".to_string() => "red".to_string(),
                },
            ),
        ]))
    );
}
