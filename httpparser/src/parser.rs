type Pos = (usize, usize, usize); // index, row, col

#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    Str(Pos, String),        // "..." or '...' (no support for quoted entities)
    Identifier(Pos, String), // ascii string starting with a letter
}

#[derive(Debug, Eq, PartialEq)]
pub enum NodeType {
    Html,
    Text,
    Head,
    Body,
}

#[derive(Debug, Eq, PartialEq)]
pub struct DomNode {
    pub node_type: NodeType,
    pub attrs: Vec<(Token, Token)>,
    pub pos: Pos,
    pub children: Vec<DomNode>,
}

impl DomNode {
    fn new(
        node_type: NodeType,
        attrs: Vec<(Token, Token)>,
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

fn consume_whitespace(input: &Vec<char>, (mut index, mut row, mut col): Pos) -> Result<Pos, Error> {
    if index >= input.len() {
        return Err(Error::Eof((index, row, col)));
    }
    while index < input.len() && input[index].is_ascii_whitespace() {
        match input[index] {
            ' ' | '\t' => col += 1,
            '\n' => {
                row += 1;
                col = 1;
            }
            _ => {
                return Err(Error::Unexpected(
                    (index, row, col),
                    format!("unsupported whitespace char: {}", input[index]).to_string(),
                ))
            }
        }
        index += 1;
    }
    Ok((index, row, col))
}

fn parse_identifier(input: &Vec<char>, pos: Pos) -> Result<(Token, Pos), Error> {
    let pos = consume_whitespace(input, pos)?;
    parse_identifier_strict(&input, pos)
}

fn parse_identifier_strict(
    input: &Vec<char>,
    (mut index, row, mut col): Pos,
) -> Result<(Token, Pos), Error> {
    if index >= input.len() {
        return Err(Error::Eof((index, row, col)));
    }
    if !input[index].is_ascii_alphabetic() {
        return Err(Error::Unexpected(
            (index, row, col),
            "expected identifier".to_string(),
        ));
    }
    let mut id: Vec<char> = vec![input[index]];
    let start_pos = (index, row, col);
    index += 1;
    col += 1;
    while index < input.len() && input[index].is_ascii_alphanumeric() {
        id.push(input[index]);
        index += 1;
        col += 1;
    }
    Ok((
        Token::Identifier(start_pos, id.into_iter().collect()),
        (index, row, col),
    ))
}

// Returned position is the start of the first quote char. String returned in Token::Str enum does not include quotes.
fn parse_string(input: &Vec<char>, pos: Pos) -> Result<(Token, Pos), Error> {
    let (mut index, row, mut col) = consume_whitespace(input, pos)?;
    if index >= input.len() {
        return Err(Error::Eof((index, row, col)));
    }
    if input[index] != '\'' && input[index] != '"' {
        return Err(Error::Unexpected(
            (index, row, col),
            "expected quote".to_string(),
        ));
    }
    let quote = input[index];
    let mut st: Vec<char> = vec![];
    let start_pos = (index, row, col);
    index += 1;
    col += 1;
    while index < input.len() && input[index] != quote {
        if input[index] == '\n' {
            // Also include \r?
            return Err(Error::Unexpected(
                (index, row, col),
                "unexpected newline in string".to_string(),
            ));
        }
        st.push(input[index]);
        index += 1;
        col += 1;
    }
    if index == input.len() {
        Err(Error::Unexpected(
            (index, row, col),
            "unexpected EOF when parsing string".to_string(),
        ))
    } else {
        // end of string
        index += 1;
        col += 1;
        Ok((
            Token::Str(start_pos, st.into_iter().collect()),
            (index, row, col),
        ))
    }
}

fn parse_one_char(input: &Vec<char>, pos: Pos, ch: char) -> Result<Pos, Error> {
    let (index, row, col) = consume_whitespace(input, pos)?;
    if input[index] == ch {
        Ok((index + 1, row, col + 1))
    } else {
        Err(Error::Unexpected(
            pos,
            format!("expected {}, got {}", ch, input[index]).to_string(),
        ))
    }
}

fn parse_tag_attributes(input: &Vec<char>, pos: Pos) -> Result<(Vec<(Token, Token)>, Pos), Error> {
    let mut next_start_pos = pos;
    let mut id_start_pos = pos;
    let mut attributes: Vec<(Token, Token)> = vec![];
    loop {
        match parse_identifier(input, id_start_pos) {
            Ok((id, eq_start_pos)) => match parse_one_char(input, eq_start_pos, '=') {
                Ok(val_start_pos) => match parse_string(input, val_start_pos) {
                    Ok((val, next_id_start_pos)) => {
                        attributes.push((id, val));
                        id_start_pos = next_id_start_pos;
                        next_start_pos = next_id_start_pos;
                    }
                    Err(_) => return Ok((attributes, next_start_pos)),
                },
                Err(_) => return Ok((attributes, next_start_pos)),
            },
            Err(_) => return Ok((attributes, next_start_pos)),
        }
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

fn parse_opening_tag(input: &Vec<char>, pos: Pos) -> Result<(DomNode, Pos), Error> {
    let pos = parse_one_char(&input, pos, '<')?;
    let (index, row, col) = pos;
    let tag_start_pos = (index - 1, row, col - 1);
    let (tag_id, pos) = parse_identifier_strict(&input, pos)?;
    let node_type = match tag_id {
        Token::Identifier(tag_id_pos, tag_id_str) => match tag_id_str_to_node_type(&tag_id_str) {
            Some(node_type) => node_type,
            None => {
                return Err(Error::Unexpected(
                    tag_id_pos,
                    format!("unexpected node type: {}", tag_id_str).to_string(),
                ))
            }
        },
        _ => unreachable!(),
    };
    let (attrs, pos) = parse_tag_attributes(&input, pos)?;
    let pos = match parse_one_char(&input, pos, '>') {
        Ok(pos) => pos,
        Err(Error::Eof(_)) => {
            return Err(Error::Unexpected(
                tag_start_pos,
                format!("unclosed tag: {:?}", node_type),
            ))
        }
        Err(err) => return Err(err),
    };
    Ok((DomNode::new(node_type, attrs, tag_start_pos, vec![]), pos))
}

fn parse_closing_tag(
    input: &Vec<char>,
    pos: Pos,
    opening_tag: DomNode,
) -> Result<(DomNode, Pos), Error> {
    let pos = parse_one_char(&input, pos, '<')?;
    let (index, row, col) = pos;
    let tag_start_pos = (index - 1, row, col - 1);
    let pos = parse_one_char(&input, pos, '/')?;
    let (tag_id, pos) = parse_identifier_strict(&input, pos)?;
    let node_type = match tag_id {
        Token::Identifier(tag_id_pos, tag_id_str) => match tag_id_str_to_node_type(&tag_id_str) {
            Some(node_type) => node_type,
            None => {
                return Err(Error::Unexpected(
                    tag_id_pos,
                    format!("unexpected node type: {}", tag_id_str).to_string(),
                ))
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
    let pos = parse_one_char(&input, pos, '>')?;
    Ok((opening_tag, pos))
}

fn parse_text_node(
    input: &Vec<char>,
    (mut index, mut row, mut col): Pos,
) -> Result<(DomNode, Pos), Error> {
    let mut text: Vec<char> = vec![];
    let start_pos = (index, row, col);
    while index < input.len() && input[index] != '<' {
        text.push(input[index]);
        index += 1;
        match input[index] {
            '\n' => {
                row += 1;
                col = 1;
            }
            _ => col += 1,
        }
    }
    Ok((
        DomNode::new(NodeType::Text, vec![], start_pos, vec![]),
        (index, row, col),
    ))
}

fn parse_node(input: &Vec<char>, pos: Pos) -> Result<(DomNode, Pos), Error> {
    let (mut node, mut pos) = parse_opening_tag(&input, pos)?;
    let node_start_pos = node.pos;
    loop {
        pos = match consume_whitespace(&input, pos) {
            Ok(pos) => pos,
            Err(Error::Eof(_)) => {
                return Err(Error::Unexpected(
                    node_start_pos,
                    format!("unclosed element: {:?}", node),
                ))
            }
            Err(e) => return Err(e),
        };
        let (index, _, _) = pos;
        if index >= input.len() {
            return Err(Error::Unexpected(
                node_start_pos,
                format!("unclosed element: {:?}", node),
            ));
        }
        match input[index] {
            '<' => {
                if index + 1 >= input.len() {
                    return Err(Error::Unexpected(
                        node_start_pos,
                        format!("unclosed element: {:?}", node),
                    ));
                }
                if input[index + 1] == '/' {
                    return parse_closing_tag(&input, pos, node);
                }
                let (child_node, new_pos) = parse_node(&input, pos)?;
                node.children.push(child_node);
                pos = new_pos;
            }
            _ => {
                let (text_node, new_pos) = parse_text_node(&input, pos)?;
                node.children.push(text_node);
                pos = new_pos;
            }
        }
    }
}

pub fn parse(input: &String) -> Result<DomNode, Error> {
    let input = input.chars().collect();
    let pos = (0, 1, 1);
    let (node, _) = parse_node(&input, pos)?;
    Ok(node)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_identifier1() {
        let input = "asdf";
        assert_eq!(
            parse_identifier(&input.chars().collect(), (0, 1, 1)),
            Ok((Token::Identifier((0, 1, 1), "asdf".to_string()), (4, 1, 5)))
        );
    }

    #[test]
    fn test_parse_identifier2() {
        let input = "a1s2f";
        assert_eq!(
            parse_identifier(&input.chars().collect(), (0, 1, 1)),
            Ok((Token::Identifier((0, 1, 1), "a1s2f".to_string()), (5, 1, 6)))
        );
    }

    #[test]
    fn test_parse_identifier_ignores_whitespace() {
        let input = " asdf";
        assert_eq!(
            parse_identifier(&input.chars().collect(), (0, 1, 1)),
            Ok((Token::Identifier((1, 1, 2), "asdf".to_string()), (5, 1, 6)))
        );
    }

    #[test]
    fn test_parse_identifier_fail1() {
        let input = "1sdf";
        assert_eq!(
            parse_identifier(&input.chars().collect(), (0, 1, 1)),
            Err(Error::Unexpected(
                (0, 1, 1),
                "expected identifier".to_string()
            ))
        );
    }

    #[test]
    fn test_parse_string_double_quote() {
        let input = "\"asdf\"";
        assert_eq!(
            parse_string(&input.chars().collect(), (0, 1, 1)),
            Ok((Token::Str((0, 1, 1), "asdf".to_string()), (6, 1, 7)))
        );
    }

    #[test]
    fn test_parse_string_single_quote() {
        let input = "'asdf'";
        assert_eq!(
            parse_string(&input.chars().collect(), (0, 1, 1)),
            Ok((Token::Str((0, 1, 1), "asdf".to_string()), (6, 1, 7)))
        );
    }

    #[test]
    fn test_parse_string_fail_no_closing_quote() {
        let input = "'asdf";
        assert_eq!(
            parse_string(&input.chars().collect(), (0, 1, 1)),
            Err(Error::Unexpected(
                (5, 1, 6),
                "unexpected EOF when parsing string".to_string()
            ))
        );
    }

    #[test]
    fn test_parse_string_fail_wrong_closing_quote() {
        let input = "'asdf\"";
        assert_eq!(
            parse_string(&input.chars().collect(), (0, 1, 1)),
            Err(Error::Unexpected(
                (6, 1, 7),
                "unexpected EOF when parsing string".to_string()
            ))
        );
    }

    #[test]
    fn test_parse_tag_attributes() {
        let input = "a=\"a\" b=\"b\"";
        assert_eq!(
            parse_tag_attributes(&input.chars().collect(), (0, 1, 1)),
            Ok((
                vec![
                    (
                        Token::Identifier((0, 1, 1), "a".to_string()),
                        Token::Str((2, 1, 3), "a".to_string()),
                    ),
                    (
                        Token::Identifier((6, 1, 7), "b".to_string()),
                        Token::Str((8, 1, 9), "b".to_string()),
                    ),
                ],
                (11, 1, 12)
            ))
        )
    }

    #[test]
    fn test_parse_tag_attributes_whitespace() {
        let input = indoc!(
            "
            a   =\t\"a\"
            b=
            \"b\"
        "
        );
        assert_eq!(
            parse_tag_attributes(&input.chars().collect(), (0, 1, 1)),
            Ok((
                vec![
                    (
                        Token::Identifier((0, 1, 1), "a".to_string()),
                        Token::Str((6, 1, 7), "a".to_string()),
                    ),
                    (
                        Token::Identifier((10, 2, 1), "b".to_string()),
                        Token::Str((13, 3, 1), "b".to_string()),
                    ),
                ],
                (16, 3, 4)
            ))
        )
    }

    #[test]
    fn test_parse_tag_attributes_early_termination1() {
        let input = "a='a' <";
        assert_eq!(
            parse_tag_attributes(&input.chars().collect(), (0, 1, 1)),
            Ok((
                vec![
                    (
                        Token::Identifier((0, 1, 1), "a".to_string()),
                        Token::Str((2, 1, 3), "a".to_string()),
                    ),
                ],
                (5, 1, 6)
            ))
        );
    }

    #[test]
    fn test_parse_tag_attributes_early_termination2() {
        let input = "a='a <";
        assert_eq!(
            parse_tag_attributes(&input.chars().collect(), (0, 1, 1)),
            Ok((vec![], (0, 1, 1)))
        );
    }

    #[test]
    fn test_parse_opening_tag() {
        let input = "<html>".to_string();
        assert_eq!(
            parse_opening_tag(&input.chars().collect(), (0, 1, 1)),
            Ok((
                DomNode::new(NodeType::Html, vec![], (0, 1, 1), vec![]),
                (6, 1, 7)
            ))
        );
    }

    #[test]
    fn test_parse_opening_tag_strict_whitespace() {
        let input = "< html>".to_string();
        assert_eq!(
            parse_opening_tag(&input.chars().collect(), (0, 1, 1)),
            Err(Error::Unexpected(
                (1, 1, 2),
                "expected identifier".to_string()
            ))
        );
    }

    #[test]
    fn test_parse1() {
        let input = "<html></html>".to_string();
        assert_eq!(
            parse(&input),
            Ok(DomNode::new(NodeType::Html, vec![], (0, 1, 1), vec![]))
        );
    }

    #[test]
    fn test_parse_text_node1() {
        let input = "<html>hello</html>".to_string();
        assert_eq!(
            parse(&input),
            Ok(DomNode::new(
                NodeType::Html,
                vec![],
                (0, 1, 1),
                vec![DomNode::new(NodeType::Text, vec![], (6, 1, 7), vec![])]
            ))
        );
    }

    #[test]
    fn test_parse_text_node2() {
        let input = "<html> hello    </html>".to_string();
        assert_eq!(
            parse(&input),
            Ok(DomNode::new(
                NodeType::Html,
                vec![],
                (0, 1, 1),
                vec![DomNode::new(NodeType::Text, vec![], (7, 1, 8), vec![])]
            ))
        );
    }

    #[test]
    fn test_parse_multiple_nested_nodes() {
        let input = "<html> hello hello <body></body>  </html>".to_string();
        assert_eq!(
            parse(&input),
            Ok(DomNode::new(
                NodeType::Html,
                vec![],
                (0, 1, 1),
                vec![
                    DomNode::new(NodeType::Text, vec![], (7, 1, 8), vec![]),
                    DomNode::new(NodeType::Body, vec![], (19, 1, 20), vec![]),
                ]
            ))
        );
    }

    #[test]
    fn test_parse_multiple_nested_nodes_attrs() {
        let input = "<html>  <body a='b'  c=\"d\"></body>  </html>".to_string();
        assert_eq!(
            parse(&input),
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
                                Token::Str((16, 1, 17), "b".to_string()),
                            ),
                            (
                                Token::Identifier((21, 1, 22), "c".to_string()),
                                Token::Str((23, 1, 24), "d".to_string()),
                            ),
                        ],
                        (8, 1, 9),
                        vec![],
                    ),
                ]
            ))
        );
    }

    #[test]
    fn test_parse_fail_eof1() {
        let input = "<html".to_string();
        assert_eq!(
            parse(&input),
            Err(Error::Unexpected(
                (0, 1, 1),
                format!("unclosed tag: {:?}", NodeType::Html)
            ))
        )
    }

    #[test]
    fn test_parse_fail_eof2() {
        let input = "<html>".to_string();
        assert_eq!(
            parse(&input),
            Err(Error::Unexpected(
                (0, 1, 1),
                format!(
                    "unclosed element: {:?}",
                    DomNode::new(NodeType::Html, vec![], (0, 1, 1), vec![])
                )
            ))
        )
    }

    #[test]
    fn test_parse_fail_mismatched_closing_tag() {
        let input = "<html></body>".to_string();
        assert_eq!(
            parse(&input),
            Err(Error::Unexpected(
                (6, 1, 7),
                format!(
                    "expected closing tag for {:?}, got {:?}",
                    NodeType::Html,
                    NodeType::Body
                )
            ))
        )
    }
}
