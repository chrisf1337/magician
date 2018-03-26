type Pos = (usize, usize, usize); // index, row, col

#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    Str(Pos, String),        // "..." or '...' (no support for quoted entities)
    Identifier(Pos, String), // ascii string starting with a letter
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
    let (mut index, row, mut col) = consume_whitespace(input, pos)?;
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

// pub fn parse(input: &String) -> Result<Vec<Token>, (Error, Vec<Token>)> {
//     let input = input.chars().collect();
//     let mut pos = (0, 1, 1);
//     let mut tokens: Vec<Token> = vec![];
//     loop {
//         match parse_one_token(&input, pos) {
//             Err(Error::Eof(_)) => return Ok(tokens),
//             Err(err) => return Err((err, tokens)),
//             Ok((Token::Space(_), new_pos)) | Ok((Token::Newline(_), new_pos)) => pos = new_pos,
//             Ok((Token::LeftAngleBracket(tag_start_pos), tag_name_start_pos)) => {
//                 match parse_identifier(input, tag_name_start_pos) {
//                     Ok(Token::Identifier(tag_name_pos, tag_name), tag_name_end_pos) =>
//                     Ok(_) => unreachable!(),
//                 }
//             }
//             Ok((token, new_pos)) => {
//                 tokens.push(token);
//                 pos = new_pos;
//             }
//         }
//     }
// }

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
    fn test_parse_tag_attributes_early_termination() {
        let input = "a='a <";
        assert_eq!(
            parse_tag_attributes(&input.chars().collect(), (0, 1, 1)),
            Ok((vec![], (0, 1, 1)))
        );
    }

    // #[test]
    // fn test_parse1() {
    //     let input = "<html>".to_string();
    //     assert_eq!(
    //         parse(&input),
    //         Ok(vec![
    //             Token::LeftAngleBracket((0, 1, 1)),
    //             Token::Identifier((1, 1, 2), "html".to_string()),
    //             Token::RightAngleBracket((5, 1, 6)),
    //         ])
    //     );
    // }

    // #[test]
    // fn test_parse2() {
    //     let input = "</html>".to_string();
    //     assert_eq!(
    //         parse(&input),
    //         Ok(vec![
    //             Token::LeftAngleBracket((0, 1, 1)),
    //             Token::Slash((1, 1, 2)),
    //             Token::Identifier((2, 1, 3), "html".to_string()),
    //             Token::RightAngleBracket((6, 1, 7)),
    //         ])
    //     );
    // }

    // #[test]
    // fn test_parse3() {
    //     let input = "<div class=\"test\">".to_string();
    //     //           01   5    1 1      1
    //     //                     0 1      7
    //     assert_eq!(
    //         parse(&input),
    //         Ok(vec![
    //             Token::LeftAngleBracket((0, 1, 1)),
    //             Token::Identifier((1, 1, 2), "div".to_string()),
    //             Token::Identifier((5, 1, 6), "class".to_string()),
    //             Token::Equal((10, 1, 11)),
    //             Token::Str((11, 1, 12), "test".to_string()),
    //             Token::RightAngleBracket((17, 1, 18)),
    //         ])
    //     );
    // }

    // #[test]
    // fn test_parse_ignores_whitespace() {
    //     let input = "   asdf".to_string();
    //     assert_eq!(
    //         parse(&input),
    //         Ok(vec![Token::Identifier((3, 1, 4), "asdf".to_string())])
    //     );
    // }

    // #[test]
    // fn test_parse_increments_row_on_newline() {
    //     let input = indoc!(
    //         "
    //         asdf
    //         asdf
    //     "
    //     ).to_string();
    //     assert_eq!(
    //         parse(&input),
    //         Ok(vec![
    //             Token::Identifier((0, 1, 1), "asdf".to_string()),
    //             Token::Identifier((5, 2, 1), "asdf".to_string()),
    //         ])
    //     );
    // }
}
