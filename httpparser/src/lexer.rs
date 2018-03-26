type Pos = (usize, usize, usize); // index, row, col

#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    LeftAngleBracket(Pos),   // <
    RightAngleBracket(Pos),  // >
    Slash(Pos),              // /
    Equal(Pos),              // =
    Str(Pos, String),        // "..." or '...' (no support for quoted entities)
    Identifier(Pos, String), // ascii string starting with a letter
    Space(Pos),
    Newline(Pos),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Error {
    Eof(Pos),
    Unexpected(Pos, String),
}

fn lex_identifier(
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
fn lex_string(input: &Vec<char>, (mut index, row, mut col): Pos) -> Result<(Token, Pos), Error> {
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
            "unexpected EOF when lexing string".to_string(),
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

fn lex_one_token(input: &Vec<char>, (index, row, col): Pos) -> Result<(Token, Pos), Error> {
    if index >= input.len() {
        return Err(Error::Eof((index, row, col)));
    }
    match input[index] {
        '<' => Ok((
            Token::LeftAngleBracket((index, row, col)),
            (index + 1, row, col + 1),
        )),
        '>' => Ok((
            Token::RightAngleBracket((index, row, col)),
            (index + 1, row, col + 1),
        )),
        '/' => Ok((Token::Slash((index, row, col)), (index + 1, row, col + 1))),
        '=' => Ok((Token::Equal((index, row, col)), (index + 1, row, col + 1))),
        ' ' => Ok((Token::Space((index, row, col)), (index + 1, row, col + 1))),
        '\n' => Ok((Token::Newline((index, row, col)), (index + 1, row + 1, 1))),
        '\'' | '"' => lex_string(input, (index, row, col)),
        c => {
            if c.is_ascii_alphabetic() {
                lex_identifier(input, (index, row, col))
            } else {
                Err(Error::Unexpected((index, row, col), c.to_string()))
            }
        }
    }
}

pub fn lex(input: &String) -> Result<Vec<Token>, (Error, Vec<Token>)> {
    let input = input.chars().collect();
    let mut pos = (0, 1, 1);
    let mut tokens: Vec<Token> = vec![];
    loop {
        match lex_one_token(&input, pos) {
            Err(Error::Eof(_)) => return Ok(tokens),
            Err(err) => return Err((err, tokens)),
            Ok((Token::Space(_), new_pos)) | Ok((Token::Newline(_), new_pos)) => pos = new_pos,
            Ok((token, new_pos)) => {
                tokens.push(token);
                pos = new_pos;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_identifier1() {
        let input = "asdf";
        assert_eq!(
            lex_identifier(&input.chars().collect(), (0, 1, 1)),
            Ok((Token::Identifier((0, 1, 1), "asdf".to_string()), (4, 1, 5)))
        );
    }

    #[test]
    fn test_lex_identifier2() {
        let input = "a1s2f";
        assert_eq!(
            lex_identifier(&input.chars().collect(), (0, 1, 1)),
            Ok((Token::Identifier((0, 1, 1), "a1s2f".to_string()), (5, 1, 6)))
        );
    }

    #[test]
    fn test_lex_identifier_fail1() {
        let input = " asdf";
        assert_eq!(
            lex_identifier(&input.chars().collect(), (0, 1, 1)),
            Err(Error::Unexpected(
                (0, 1, 1),
                "expected identifier".to_string()
            ))
        );
    }

    #[test]
    fn test_lex_identifier_fail2() {
        let input = "1sdf";
        assert_eq!(
            lex_identifier(&input.chars().collect(), (0, 1, 1)),
            Err(Error::Unexpected(
                (0, 1, 1),
                "expected identifier".to_string()
            ))
        );
    }

    #[test]
    fn test_lex_one_token1() {
        let input = "<html>";
        assert_eq!(
            lex_one_token(&input.chars().collect(), (0, 1, 1)),
            Ok((Token::LeftAngleBracket((0, 1, 1)), (1, 1, 2)))
        );
    }

    #[test]
    fn test_lex_one_token2() {
        let input = "<html>";
        assert_eq!(
            lex_one_token(&input.chars().collect(), (1, 1, 2)),
            Ok((Token::Identifier((1, 1, 2), "html".to_string()), (5, 1, 6)))
        );
    }

    #[test]
    fn test_lex_one_token3() {
        let input = "<html>";
        assert_eq!(
            lex_one_token(&input.chars().collect(), (5, 1, 6)),
            Ok((Token::RightAngleBracket((5, 1, 6)), (6, 1, 7)))
        );
    }

    #[test]
    fn test_lex_string_double_quote() {
        let input = "\"asdf\"";
        assert_eq!(
            lex_string(&input.chars().collect(), (0, 1, 1)),
            Ok((Token::Str((0, 1, 1), "asdf".to_string()), (6, 1, 7)))
        );
    }

    #[test]
    fn test_lex_string_single_quote() {
        let input = "'asdf'";
        assert_eq!(
            lex_string(&input.chars().collect(), (0, 1, 1)),
            Ok((Token::Str((0, 1, 1), "asdf".to_string()), (6, 1, 7)))
        );
    }

    #[test]
    fn test_lex_string_fail_no_closing_quote() {
        let input = "'asdf";
        assert_eq!(
            lex_string(&input.chars().collect(), (0, 1, 1)),
            Err(Error::Unexpected(
                (5, 1, 6),
                "unexpected EOF when lexing string".to_string()
            ))
        );
    }

    #[test]
    fn test_lex_string_fail_wrong_closing_quote() {
        let input = "'asdf\"";
        assert_eq!(
            lex_string(&input.chars().collect(), (0, 1, 1)),
            Err(Error::Unexpected(
                (6, 1, 7),
                "unexpected EOF when lexing string".to_string()
            ))
        );
    }

    #[test]
    fn test_lex1() {
        let input = "<html>".to_string();
        assert_eq!(
            lex(&input),
            Ok(vec![
                Token::LeftAngleBracket((0, 1, 1)),
                Token::Identifier((1, 1, 2), "html".to_string()),
                Token::RightAngleBracket((5, 1, 6)),
            ])
        );
    }

    #[test]
    fn test_lex2() {
        let input = "</html>".to_string();
        assert_eq!(
            lex(&input),
            Ok(vec![
                Token::LeftAngleBracket((0, 1, 1)),
                Token::Slash((1, 1, 2)),
                Token::Identifier((2, 1, 3), "html".to_string()),
                Token::RightAngleBracket((6, 1, 7)),
            ])
        );
    }

    #[test]
    fn test_lex3() {
        let input = "<div class=\"test\">".to_string();
        //           01   5    1 1      1
        //                     0 1      7
        assert_eq!(
            lex(&input),
            Ok(vec![
                Token::LeftAngleBracket((0, 1, 1)),
                Token::Identifier((1, 1, 2), "div".to_string()),
                Token::Identifier((5, 1, 6), "class".to_string()),
                Token::Equal((10, 1, 11)),
                Token::Str((11, 1, 12), "test".to_string()),
                Token::RightAngleBracket((17, 1, 18)),
            ])
        );
    }

    #[test]
    fn test_lex_ignores_whitespace() {
        let input = "   asdf".to_string();
        assert_eq!(
            lex(&input),
            Ok(vec![Token::Identifier((3, 1, 4), "asdf".to_string())])
        );
    }

    #[test]
    fn test_lex_increments_row_on_newline() {
        let input = indoc!(
            "
            asdf
            asdf
        "
        ).to_string();
        assert_eq!(
            lex(&input),
            Ok(vec![
                Token::Identifier((0, 1, 1), "asdf".to_string()),
                Token::Identifier((5, 2, 1), "asdf".to_string()),
            ])
        );
    }
}
