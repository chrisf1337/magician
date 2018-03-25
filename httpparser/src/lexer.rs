type Pos = (usize, usize, usize); // index, row, col

#[derive(Debug, Eq, PartialEq)]
pub enum Token {
    LeftAngleBracket,  // <
    RightAngleBracket, // >
    Identifier(String),
}

#[derive(Debug, Eq, PartialEq)]
pub enum Error {
    Eof(Pos),
    Unexpected(Pos, String),
}

pub fn lex_identifier(
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
    let mut id: Vec<char> = vec![];
    while index < input.len() && input[index].is_ascii_alphabetic() {
        id.push(input[index]);
        index += 1;
        col += 1;
    }
    Ok((
        Token::Identifier(id.into_iter().collect()),
        (index, row, col),
    ))
}

pub fn lex_one_token(input: &Vec<char>, (index, row, col): Pos) -> Result<(Token, Pos), Error> {
    if index >= input.len() {
        return Err(Error::Eof((index, row, col)));
    }
    match input[index] {
        '<' => Ok((Token::LeftAngleBracket, (index + 1, row, col + 1))),
        '>' => Ok((Token::RightAngleBracket, (index + 1, row, col + 1))),
        _ => Err(Error::Unexpected((index, row, col), "".to_string())),
    }
}

pub fn lex(input: &String) {
    lex_one_token(&input.chars().collect(), (0, 1, 1));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_identifier1() {
        let input = "asdf";
        assert_eq!(
            lex_identifier(&input.chars().collect(), (0, 1, 1)),
            Ok((Token::Identifier("asdf".to_string()), (4, 1, 5)))
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
}
