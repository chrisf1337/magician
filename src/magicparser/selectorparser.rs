use magicparser::error::Error;
use magicparser::lexer::Lexer;
use magicparser::parser::Parser;
use magicparser::{ElemType, Pos, Token};
use std::boxed::Box;
use std::convert::From;
use std::result;

#[derive(Debug, Eq, PartialEq)]
enum SelectorParserError {
    Eof(Pos),
    Unexpected(Pos, String),
    MultipleIds(Pos, String),
}

impl From<SelectorParserError> for Error {
    fn from(error: SelectorParserError) -> Self {
        match error {
            SelectorParserError::Eof(pos) => Error::Eof(pos),
            SelectorParserError::Unexpected(pos, msg) => Error::Unexpected(pos, msg),
            SelectorParserError::MultipleIds(pos, ids) => {
                Error::Unexpected(pos, format!("{:?}", ids))
            }
        }
    }
}

impl From<Error> for SelectorParserError {
    fn from(error: Error) -> Self {
        match error {
            Error::Eof(pos) => SelectorParserError::Eof(pos),
            Error::Unexpected(pos, msg) => SelectorParserError::Unexpected(pos, msg),
        }
    }
}

type Result<T> = result::Result<T, SelectorParserError>;
type ParserFn<T> = fn(&mut SelectorParser) -> Result<T>;

#[derive(Debug, Eq, PartialEq)]
pub(super) enum AttrSelectorOp {
    Exactly(Pos),            // =
    ExactlyOne(Pos),         // ~=
    ExactlyOrHyphen(Pos),    // |=
    Prefixed(Pos),           // ^=
    Suffixed(Pos),           // $=
    ContainsAtLeastOne(Pos), // *=
}

#[derive(Debug, Eq, PartialEq)]
pub(super) struct SimpleSelector {
    pub pos: Pos,
    pub elem_type: Option<ElemType>,
    pub id: Option<Token>,   // AttrIdentifier or Str
    pub classes: Vec<Token>, // AttrIdentifier or Str
    pub universal: bool,
}

impl SimpleSelector {
    pub fn new(
        pos: Pos,
        elem_type: Option<ElemType>,
        id: Option<Token>,
        classes: Vec<Token>,
        universal: bool,
    ) -> SimpleSelector {
        SimpleSelector {
            pos,
            elem_type,
            id,
            classes,
            universal,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub(super) struct AttrSelector {
    pub pos: Pos,
    pub attr: Token,
    pub op_val: Option<(AttrSelectorOp, Token)>,
    pub case_insensitive: bool,
}

impl AttrSelector {
    pub(super) fn new(
        pos: Pos,
        attr: Token,
        op_val: Option<(AttrSelectorOp, Token)>,
        case_insensitive: bool,
    ) -> AttrSelector {
        AttrSelector {
            pos,
            attr,
            op_val,
            case_insensitive,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub(super) enum NthExprOp {
    Add(Pos),
    Sub(Pos),
}

#[derive(Debug, Eq, PartialEq)]
pub(super) enum NthExpr {
    A(Pos, Token),
    AnOpB(Pos, Option<Token>, Option<NthExprOp>, Option<Token>),
    Even(Pos),
    Odd(Pos),
}

#[derive(Debug, Eq, PartialEq)]
pub(super) enum PseudoClassSelector {
    Active(Pos),
    FirstChild(Pos),
    FirstOfType(Pos),
    Hover(Pos),
    // experimental: Dir,
    // experimental: Host,
    // experimental: HostContext,
    Lang(Pos, Token),
    LastChild(Pos),
    LastOfType(Pos),
    Link(Pos),
    Matches(Pos, Box<Selector>),
    Visited(Pos),
    Not(Pos, Box<Selector>),
    NthChild(Pos, NthExpr),
    NthLastChild(Pos, NthExpr),
    NthLastOfType(Pos, NthExpr),
    NthOfType(Pos, NthExpr),
}

#[derive(Debug, Eq, PartialEq)]
pub(super) enum PseudoElementSelector {
    After(Pos),
    Before(Pos),
    Cue(Pos),
    FirstLetter(Pos),
    FirstLine(Pos),
    Selection(Pos),
    Slotted(Pos),
}

#[derive(Debug, Eq, PartialEq)]
pub(super) enum Combinator {
    AdjacentSibling(Pos), // +
    GeneralSibling(Pos),  // ~
    Child(Pos),           // >
    Descendant(Pos),      // space
}

#[derive(Debug, Eq, PartialEq)]
pub(super) enum Selector {
    Simple(SimpleSelector),
    Attr(AttrSelector),
    PseudoClass(PseudoClassSelector),
    PseudoElement(PseudoElementSelector),
    Seq(Vec<Selector>), // AND of selectors (e.g. a#id[href="www.example.com"]:visited )
    Combinator(Box<Selector>, Combinator, Box<Selector>),
    Group(Vec<Selector>), // comma-separated group
}

pub(super) struct SelectorParser {
    lexer: Lexer,
}

impl SelectorParser {
    fn new(input: &str) -> SelectorParser {
        Self::new_with_pos(input, (0, 1, 1))
    }

    fn new_with_pos(input: &str, pos: Pos) -> SelectorParser {
        SelectorParser {
            lexer: Lexer::new_with_pos(input, pos, "", ""),
        }
    }

    // CssParser should have trimmed whitespace from the selector string, so we
    // don't have to worry about consuming whitespace. In effect, this makes
    // this parser strict.
    fn parse_simple_selector(&mut self) -> Result<Selector> {
        let start_pos = self.pos();
        let mut elem_type: Option<ElemType> = None;
        let mut classes = vec![];
        let mut id: Option<Token> = None;
        let mut universal = false;
        let mut found = false;
        loop {
            match self.lexer.peek_char() {
                Ok((_, '.')) => {
                    self.lexer.consume_char()?;
                    let class = self.parse_attr_identifier_strict()?;
                    classes.push(class);
                    found = true;
                }
                Ok((_, '#')) => {
                    self.lexer.consume_char()?;
                    let new_id = self.parse_attr_identifier_strict()?;
                    if let Some(old_id) = id {
                        return Err(SelectorParserError::MultipleIds(
                            start_pos,
                            format!("{:?} {:?}", old_id, new_id),
                        ));
                    }
                    id = Some(new_id);
                    found = true;
                }
                Ok((_, '*')) => {
                    self.lexer.consume_char()?;
                    universal = true;
                    found = true;
                }
                Ok((_, _)) => match self.parse_elem_identifier_strict() {
                    Ok(Token::ElemIdentifier(_, elem_name)) => {
                        elem_type = Some(ElemType::from(&elem_name));
                        found = true;
                    }
                    Ok(_) => unreachable!(),
                    Err(_) => break,
                },

                Err(_) => break,
            }
        }
        if !found {
            // empty selector
            Err(SelectorParserError::Unexpected(
                start_pos,
                "empty simple selector".to_string(),
            ))
        } else {
            Ok(Selector::Simple(SimpleSelector::new(
                start_pos, elem_type, id, classes, universal,
            )))
        }
    }

    fn parse_attr_selector_op(&mut self) -> Result<AttrSelectorOp> {
        self.lexer.consume_whitespace()?;
        match self.lexer
            .try_parse_chars_list(vec!["=", "~=", "|=", "^=", "$=", "*="])
        {
            Ok((pos, op_str)) => Ok(match op_str.as_ref() {
                "=" => AttrSelectorOp::Exactly(pos),
                "~=" => AttrSelectorOp::ExactlyOne(pos),
                "|=" => AttrSelectorOp::ExactlyOrHyphen(pos),
                "^=" => AttrSelectorOp::Prefixed(pos),
                "$=" => AttrSelectorOp::Suffixed(pos),
                "*=" => AttrSelectorOp::ContainsAtLeastOne(pos),
                _ => unreachable!(),
            }),
            Err(err) => Err(SelectorParserError::from(err)),
        }
    }

    // Also strict (must start with [)
    fn parse_attr_selector(&mut self) -> Result<Selector> {
        let start_pos = self.pos();
        self.lexer.parse_chars_strict("[")?;
        let attr = self.parse_attr_identifier()?;
        let op = match self.parse_attr_selector_op() {
            Ok(op) => Some(op),
            Err(SelectorParserError::Unexpected(..)) => None,
            Err(err) => return Err(err),
        };
        let val = match op {
            Some(_) => {
                let parsers: Vec<ParserFn<Token>> =
                    vec![Self::parse_attr_identifier, Self::parse_string];
                Some(self.try_parsers(&parsers, "expected value or string")?)
            }
            None => None,
        };
        let case_insensitive = self.lexer.try_parse_chars_list(vec!["i", "I"]).is_ok();
        if op.is_none() {
            if case_insensitive {
                return Err(SelectorParserError::Unexpected(
                    start_pos,
                    "cannot specify i/I for attribute selector without op and val".to_string(),
                ));
            }
        }
        self.lexer.parse_chars("]")?;
        let op_val = match op {
            Some(op) => Some((op, val.unwrap())),
            None => None,
        };
        Ok(Selector::Attr(AttrSelector::new(
            start_pos,
            attr,
            op_val,
            case_insensitive,
        )))
    }

    // Returns a single selector instead of a Seq if there is only one selector
    fn parse_selector_seq(&mut self) -> Result<Selector> {
        let start_pos = self.pos();
        let mut selectors = vec![];
        // first check for a simple selector, which must come first
        if let Ok(sel) = self.parse_simple_selector() {
            selectors.push(sel);
        }
        let parsers: Vec<ParserFn<Selector>> = vec![
            Self::parse_attr_selector,
            Self::parse_pseudo_class_selector,
            Self::parse_pseudo_element_selector,
        ];
        loop {
            let selector = match self.try_parsers(&parsers, "") {
                Ok(sel) => sel,
                Err(SelectorParserError::Unexpected(..)) => break,
                Err(err) => return Err(err),
            };
            selectors.push(selector);
        }
        if selectors.is_empty() {
            Err(SelectorParserError::Unexpected(
                start_pos,
                "expected selector sequence".to_string(),
            ))
        } else if selectors.len() == 1 {
            Ok(selectors.pop().unwrap())
        } else {
            Ok(Selector::Seq(selectors))
        }
    }

    fn parse_pcs_lang_args(&mut self) -> Result<Token> {
        self.lexer.parse_chars_strict("(")?;
        let token = self.parse_elem_identifier()?;
        self.lexer.parse_chars(")")?;
        Ok(token)
    }

    fn parse_pcs_selector_list_args(&mut self) -> Result<Box<Selector>> {
        self.lexer.parse_chars_strict("(")?;
        let selectors = self.parse_selector_list()?;
        self.lexer.parse_chars(")")?;
        Ok(Box::new(selectors))
    }

    fn parse_nth_expr(&mut self) -> Result<NthExpr> {
        self.lexer.consume_whitespace()?;
        let start_pos = self.pos();
        match self.lexer.peek_char() {
            Ok((_, ch)) => if ch == '+' || ch == '-' || ch == 'n' || ch.is_ascii_digit() {
                let mut neg_sign = false;
                let a = match self.parse_number_strict() {
                    Ok(num) => Some(num),
                    Err(SelectorParserError::Unexpected(..)) => if ch == '-' {
                        neg_sign = true;
                        Some(Token::Number(start_pos, -1))
                    } else {
                        None
                    },
                    Err(err) => return Err(err),
                };
                let n = if a.is_some() {
                    match self.lexer.try_parse_chars_strict("n") {
                        Ok(_) => true,
                        Err(_) => false,
                    }
                } else {
                    match self.lexer.try_parse_chars("n") {
                        Ok(_) => true,
                        Err(_) => false,
                    }
                };
                let op = match self.lexer.parse_chars_list(vec!["+", "-"]) {
                    Ok((pos, st)) => match st.as_ref() {
                        "+" => Some(NthExprOp::Add(pos)),
                        "-" => Some(NthExprOp::Sub(pos)),
                        _ => unreachable!(),
                    },
                    _ => None,
                };
                let b = match op {
                    Some(_) => Some(self.parse_number()?),
                    None => None,
                };
                if n {
                    Ok(NthExpr::AnOpB(start_pos, a, op, b))
                } else {
                    match a {
                        Some(a) => {
                            if neg_sign || op.is_some() || b.is_some() {
                                // reject "- + 3", "-1 + 3", "-1 +", etc.
                                Err(SelectorParserError::Unexpected(
                                    start_pos,
                                    "expected an + b expression, even, or odd".to_string(),
                                ))
                            } else {
                                Ok(NthExpr::A(start_pos, a))
                            }
                        }
                        None => Err(SelectorParserError::Unexpected(
                            start_pos,
                            "expected an + b expression, even, or odd".to_string(),
                        )),
                    }
                }
            } else if ch == 'e' || ch == 'o' {
                match self.lexer.try_parse_chars_list_strict(vec!["even", "odd"]) {
                    Ok((_, st)) => match st.as_ref() {
                        "even" => Ok(NthExpr::Even(start_pos)),
                        "odd" => Ok(NthExpr::Odd(start_pos)),
                        _ => unreachable!(),
                    },
                    Err(err) => Err(SelectorParserError::from(err)),
                }
            } else {
                Err(SelectorParserError::Unexpected(
                    start_pos,
                    "expected an + b expression, even, or odd".to_string(),
                ))
            },
            Err(err) => Err(SelectorParserError::from(err)),
        }
    }

    fn parse_nth_pcs_args(&mut self) -> Result<NthExpr> {
        self.lexer.parse_chars_strict("(")?;
        let expr = self.parse_nth_expr()?;
        self.lexer.parse_chars(")")?;
        Ok(expr)
    }

    // strict
    fn parse_pseudo_class_selector(&mut self) -> Result<Selector> {
        use self::PseudoClassSelector::*;
        match self.lexer.parse_chars_strict(":") {
            Ok(pos) => {
                let sel = match self.parse_elem_identifier_strict()? {
                    Token::ElemIdentifier(_, sel_name) => {
                        match sel_name.to_ascii_lowercase().as_ref() {
                            "active" => Active(pos),
                            "first-child" => FirstChild(pos),
                            "hover" => Hover(pos),
                            "lang" => Lang(pos, self.parse_pcs_lang_args()?),
                            "last-child" => LastChild(pos),
                            "link" => Link(pos),
                            "matches" => Matches(pos, self.parse_pcs_selector_list_args()?),
                            "not" => Not(pos, self.parse_pcs_selector_list_args()?),
                            "nth-child" => NthChild(pos, self.parse_nth_pcs_args()?),
                            "nth-last-child" => NthLastChild(pos, self.parse_nth_pcs_args()?),
                            "nth-last-of-type" => NthLastOfType(pos, self.parse_nth_pcs_args()?),
                            "nth-of-type" => NthOfType(pos, self.parse_nth_pcs_args()?),
                            "visited" => Visited(pos),
                            _ => {
                                return Err(SelectorParserError::Unexpected(
                                    pos,
                                    format!("unsupported pseudo-class selector: ::{}", sel_name),
                                ))
                            }
                        }
                    }
                    _ => unreachable!(),
                };
                Ok(Selector::PseudoClass(sel))
            }
            Err(err) => Err(SelectorParserError::from(err)),
        }
    }

    fn parse_pseudo_element_selector_name(
        &mut self,
        start_pos: Pos,
    ) -> Result<PseudoElementSelector> {
        match self.parse_elem_identifier_strict()? {
            Token::ElemIdentifier(_, sel_name) => match sel_name.to_ascii_lowercase().as_ref() {
                "after" => Ok(PseudoElementSelector::After(start_pos)),
                "before" => Ok(PseudoElementSelector::Before(start_pos)),
                "cue" => Ok(PseudoElementSelector::Cue(start_pos)),
                "first-letter" => Ok(PseudoElementSelector::FirstLetter(start_pos)),
                "first-line" => Ok(PseudoElementSelector::FirstLine(start_pos)),
                "selection" => Ok(PseudoElementSelector::Selection(start_pos)),
                "slotted" => Ok(PseudoElementSelector::Slotted(start_pos)),
                _ => Err(SelectorParserError::Unexpected(
                    start_pos,
                    format!("unsupported pseudo-element selector: ::{}", sel_name),
                )),
            },
            _ => unreachable!(),
        }
    }

    fn parse_pseudo_element_selector_single_colon(&mut self) -> Result<Selector> {
        match self.lexer.parse_chars_strict(":") {
            Ok(pos) => Ok(Selector::PseudoElement(
                self.parse_pseudo_element_selector_name(pos)?,
            )),
            Err(err) => Err(SelectorParserError::from(err)),
        }
    }

    fn parse_pseudo_element_selector_double_colon(&mut self) -> Result<Selector> {
        match self.lexer.parse_chars_strict("::") {
            Ok(pos) => Ok(Selector::PseudoElement(
                self.parse_pseudo_element_selector_name(pos)?,
            )),
            Err(err) => Err(SelectorParserError::from(err)),
        }
    }

    // strict
    fn parse_pseudo_element_selector(&mut self) -> Result<Selector> {
        let start_pos = self.pos();
        match self.parse_pseudo_element_selector_single_colon() {
            ok @ Ok(_) => ok,
            Err(_) => {
                self.set_pos(start_pos);
                self.parse_pseudo_element_selector_double_colon()
            }
        }
    }

    // strict
    fn parse_combinator(&mut self) -> Result<Combinator> {
        use self::Combinator::*;
        let start_pos = self.pos();
        match self.lexer.try_parse_chars_list(vec!["+", "~", ">"]) {
            Ok((pos, st)) => match st.as_ref() {
                "+" => Ok(AdjacentSibling(pos)),
                "~" => Ok(GeneralSibling(pos)),
                ">" => Ok(Child(pos)),
                _ => unreachable!(),
            },
            Err(_) => match self.lexer.try_parse_chars_list_strict(vec![" ", "\n"]) {
                Ok((pos, _)) => Ok(Descendant(pos)),
                Err(_) => Err(SelectorParserError::Unexpected(
                    start_pos,
                    "expected combinator".to_string(),
                )),
            },
        }
    }

    fn parse_selector(&mut self) -> Result<Selector> {
        let sel1 = self.parse_selector_seq()?;
        let pos = self.pos();
        match self.parse_combinator() {
            Ok(combinator) => {
                self.lexer.consume_whitespace()?;
                let sel2 = match self.parse_selector() {
                    Ok(sel) => sel,
                    Err(_) => {
                        self.set_pos(pos);
                        return Ok(sel1);
                    }
                };
                Ok(Selector::Combinator(
                    Box::new(sel1),
                    combinator,
                    Box::new(sel2),
                ))
            }
            Err(_) => Ok(sel1),
        }
    }

    fn parse_selector_list(&mut self) -> Result<Selector> {
        let mut selectors = vec![];
        // parse_selector_seq() is strict, so consume whitespace first
        self.lexer.consume_whitespace()?;
        selectors.push(self.try(Self::parse_selector)?);
        loop {
            match self.lexer.try_parse_chars(",") {
                Ok(_) => (),
                Err(_) => break,
            }
            self.lexer.consume_whitespace()?;
            match self.try(Self::parse_selector) {
                Ok(sel) => selectors.push(sel),
                Err(err) => return Err(err),
            }
        }
        if selectors.len() == 1 {
            Ok(selectors.pop().unwrap())
        } else {
            Ok(Selector::Group(selectors))
        }
    }

    pub fn parse(input: &str, pos: Pos) -> result::Result<Selector, Error> {
        let mut parser = SelectorParser::new_with_pos(input, pos);
        let group = parser.parse_selector_list()?;
        match parser.lexer.peek_char() {
            Ok((pos, ch)) => Err(Error::Unexpected(
                pos,
                format!("unexpected char '{}' at the end of selector", ch),
            )),
            Err(Error::Eof(_)) => Ok(group),
            Err(err) => Err(err),
        }
    }
}

impl Parser<SelectorParserError> for SelectorParser {
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

    #[test]
    fn test_parse_simple_selector1() {
        let mut parser = SelectorParser::new("abcd");
        let res = parser.parse_simple_selector();
        assert_eq!(
            res,
            Ok(Selector::Simple(SimpleSelector::new(
                (0, 1, 1),
                Some(ElemType::from("abcd")),
                None,
                vec![],
                false,
            )))
        );
        assert_eq!(parser.pos(), (4, 1, 5))
    }

    #[test]
    fn test_parse_simple_selector2() {
        let mut parser = SelectorParser::new("#id");
        let res = parser.parse_simple_selector();
        assert_eq!(
            res,
            Ok(Selector::Simple(SimpleSelector::new(
                (0, 1, 1),
                None,
                Some(Token::AttrIdentifier((1, 1, 2), "id".to_string())),
                vec![],
                false,
            )))
        );
        assert_eq!(parser.pos(), (3, 1, 4))
    }

    #[test]
    fn test_parse_simple_selector3() {
        let mut parser = SelectorParser::new(".cl");
        let res = parser.parse_simple_selector();
        assert_eq!(
            res,
            Ok(Selector::Simple(SimpleSelector::new(
                (0, 1, 1),
                None,
                None,
                vec![Token::AttrIdentifier((1, 1, 2), "cl".to_string())],
                false,
            )))
        );
        assert_eq!(parser.pos(), (3, 1, 4))
    }

    #[test]
    fn test_parse_simple_selector4() {
        let mut parser = SelectorParser::new("*");
        let res = parser.parse_simple_selector();
        assert_eq!(
            res,
            Ok(Selector::Simple(SimpleSelector::new(
                (0, 1, 1),
                None,
                None,
                vec![],
                true,
            )))
        );
        assert_eq!(parser.pos(), (1, 1, 2))
    }

    #[test]
    fn test_parse_simple_selector_combined1() {
        let mut parser = SelectorParser::new("ab#id.cl1.cl2");
        let res = parser.parse_simple_selector();
        assert_eq!(
            res,
            Ok(Selector::Simple(SimpleSelector::new(
                (0, 1, 1),
                Some(ElemType::from("ab")),
                Some(Token::AttrIdentifier((3, 1, 4), "id".to_string())),
                vec![
                    Token::AttrIdentifier((6, 1, 7), "cl1".to_string()),
                    Token::AttrIdentifier((10, 1, 11), "cl2".to_string()),
                ],
                false,
            )))
        );
        assert_eq!(parser.pos(), (13, 1, 14))
    }

    #[test]
    fn test_parse_simple_selector_fail() {
        let mut parser = SelectorParser::new("-ab");
        let res = parser.parse_simple_selector();
        assert_eq!(
            res,
            Err(SelectorParserError::Unexpected(
                (0, 1, 1),
                "empty simple selector".to_string()
            ))
        );
        assert_eq!(parser.pos(), (0, 1, 1))
    }

    #[test]
    fn test_parse_simple_selector_fail_multiple_ids() {
        let mut parser = SelectorParser::new("ab#id1#id2");
        let res = parser.parse_simple_selector();
        assert_eq!(
            res,
            Err(SelectorParserError::MultipleIds(
                (0, 1, 1),
                format!(
                    "{:?} {:?}",
                    Token::AttrIdentifier((3, 1, 4), "id1".to_string()),
                    Token::AttrIdentifier((7, 1, 8), "id2".to_string())
                )
            ))
        );
        assert_eq!(parser.pos(), (10, 1, 11))
    }

    #[test]
    fn test_parse_simple_selector_before_attr_selector() {
        let mut parser = SelectorParser::new("ab#id*.cl1.cl2[attr=val]");
        let res = parser.parse_simple_selector();
        assert_eq!(
            res,
            Ok(Selector::Simple(SimpleSelector::new(
                (0, 1, 1),
                Some(ElemType::from("ab")),
                Some(Token::AttrIdentifier((3, 1, 4), "id".to_string())),
                vec![
                    Token::AttrIdentifier((7, 1, 8), "cl1".to_string()),
                    Token::AttrIdentifier((11, 1, 12), "cl2".to_string()),
                ],
                true,
            )))
        );
        assert_eq!(parser.pos(), (14, 1, 15))
    }

    #[test]
    fn test_parse_attr_selector1() {
        let mut parser = SelectorParser::new("[ abc ]");
        let res = parser.parse_attr_selector();
        assert_eq!(
            res,
            Ok(Selector::Attr(AttrSelector::new(
                (0, 1, 1),
                Token::AttrIdentifier((2, 1, 3), "abc".to_string()),
                None,
                false,
            )))
        );
        assert_eq!(parser.pos(), (7, 1, 8));
    }

    #[test]
    fn test_parse_attr_selector2() {
        let mut parser = SelectorParser::new("[ a = b i ]");
        let res = parser.parse_attr_selector();
        assert_eq!(
            res,
            Ok(Selector::Attr(AttrSelector::new(
                (0, 1, 1),
                Token::AttrIdentifier((2, 1, 3), "a".to_string()),
                Some((
                    AttrSelectorOp::Exactly((4, 1, 5)),
                    Token::AttrIdentifier((6, 1, 7), "b".to_string())
                )),
                true,
            )))
        );
        assert_eq!(parser.pos(), (11, 1, 12));
    }

    #[test]
    fn test_parse_attr_selector_fail1() {
        let mut parser = SelectorParser::new("[ a = a/ ]");
        let res = parser.parse_attr_selector();
        assert_eq!(
            res,
            Err(SelectorParserError::Unexpected(
                (7, 1, 8),
                "expected ']', got '/'".to_string()
            ))
        );
        assert_eq!(parser.pos(), (7, 1, 8));
    }

    #[test]
    fn test_parse_attr_selector_fail_case_insensitive() {
        let mut parser = SelectorParser::new("[ a i ]");
        let res = parser.parse_attr_selector();
        assert_eq!(
            res,
            Err(SelectorParserError::Unexpected(
                (0, 1, 1),
                "cannot specify i/I for attribute selector without op and val".to_string()
            ))
        );
        assert_eq!(parser.pos(), (5, 1, 6));
    }

    #[test]
    fn test_parse_attr_selector_fail_empty1() {
        let mut parser = SelectorParser::new("[]");
        let res = parser.parse_attr_selector();
        assert_eq!(
            res,
            Err(SelectorParserError::Unexpected(
                (1, 1, 2),
                "expected attribute identifier".to_string()
            ))
        );
        assert_eq!(parser.pos(), (1, 1, 2));
    }

    #[test]
    fn test_parse_selector_seq1() {
        let mut parser = SelectorParser::new("a#id[href][class~='cl']");
        let res = parser.parse_selector_seq();
        assert_eq!(
            res,
            Ok(Selector::Seq(vec![
                Selector::Simple(SimpleSelector::new(
                    (0, 1, 1),
                    Some(ElemType::from("a")),
                    Some(Token::AttrIdentifier((2, 1, 3), "id".to_string())),
                    vec![],
                    false,
                )),
                Selector::Attr(AttrSelector::new(
                    (4, 1, 5),
                    Token::AttrIdentifier((5, 1, 6), "href".to_string()),
                    None,
                    false,
                )),
                Selector::Attr(AttrSelector::new(
                    (10, 1, 11),
                    Token::AttrIdentifier((11, 1, 12), "class".to_string()),
                    Some((
                        AttrSelectorOp::ExactlyOne((16, 1, 17)),
                        Token::Str((18, 1, 19), "cl".to_string()),
                    )),
                    false,
                )),
            ]))
        );
        assert_eq!(parser.pos(), (23, 1, 24));
    }

    #[test]
    fn test_parse_selector_seq2() {
        let mut parser = SelectorParser::new("[href][class~='cl']");
        let res = parser.parse_selector_seq();
        assert_eq!(
            res,
            Ok(Selector::Seq(vec![
                Selector::Attr(AttrSelector::new(
                    (0, 1, 1),
                    Token::AttrIdentifier((1, 1, 2), "href".to_string()),
                    None,
                    false,
                )),
                Selector::Attr(AttrSelector::new(
                    (6, 1, 7),
                    Token::AttrIdentifier((7, 1, 8), "class".to_string()),
                    Some((
                        AttrSelectorOp::ExactlyOne((12, 1, 13)),
                        Token::Str((14, 1, 15), "cl".to_string()),
                    )),
                    false,
                )),
            ]))
        );
        assert_eq!(parser.pos(), (19, 1, 20));
    }

    #[test]
    fn test_parse_selector_seq3() {
        let mut parser = SelectorParser::new("[href]:nth-child(even)");
        let res = parser.parse_selector_seq();
        assert_eq!(
            res,
            Ok(Selector::Seq(vec![
                Selector::Attr(AttrSelector::new(
                    (0, 1, 1),
                    Token::AttrIdentifier((1, 1, 2), "href".to_string()),
                    None,
                    false,
                )),
                Selector::PseudoClass(PseudoClassSelector::NthChild(
                    (6, 1, 7),
                    NthExpr::Even((17, 1, 18)),
                )),
            ]))
        );
        assert_eq!(parser.pos(), (22, 1, 23));
    }

    #[test]
    fn test_parse_selector_seq4() {
        let mut parser = SelectorParser::new(":nth-child(even)[href]");
        let res = parser.parse_selector_seq();
        assert_eq!(
            res,
            Ok(Selector::Seq(vec![
                Selector::PseudoClass(PseudoClassSelector::NthChild(
                    (0, 1, 1),
                    NthExpr::Even((11, 1, 12)),
                )),
                Selector::Attr(AttrSelector::new(
                    (16, 1, 17),
                    Token::AttrIdentifier((17, 1, 18), "href".to_string()),
                    None,
                    false,
                )),
            ]))
        );
        assert_eq!(parser.pos(), (22, 1, 23));
    }

    #[test]
    fn test_parse_selector_seq5() {
        let mut parser = SelectorParser::new("[href]::after");
        let res = parser.parse_selector_seq();
        assert_eq!(
            res,
            Ok(Selector::Seq(vec![
                Selector::Attr(AttrSelector::new(
                    (0, 1, 1),
                    Token::AttrIdentifier((1, 1, 2), "href".to_string()),
                    None,
                    false,
                )),
                Selector::PseudoElement(PseudoElementSelector::After((6, 1, 7))),
            ]))
        );
        assert_eq!(parser.pos(), (13, 1, 14));
    }

    #[test]
    fn test_parse_selector_seq6() {
        let mut parser = SelectorParser::new("div#id.cl1.cl2:nth-child(even)[href='link'I]::after");
        let res = parser.parse_selector_seq();
        assert_eq!(
            res,
            Ok(Selector::Seq(vec![
                Selector::Simple(SimpleSelector::new(
                    (0, 1, 1),
                    Some(ElemType::Div),
                    Some(Token::AttrIdentifier((4, 1, 5), "id".to_string())),
                    vec![
                        Token::AttrIdentifier((7, 1, 8), "cl1".to_string()),
                        Token::AttrIdentifier((11, 1, 12), "cl2".to_string()),
                    ],
                    false,
                )),
                Selector::PseudoClass(PseudoClassSelector::NthChild(
                    (14, 1, 15),
                    NthExpr::Even((25, 1, 26)),
                )),
                Selector::Attr(AttrSelector::new(
                    (30, 1, 31),
                    Token::AttrIdentifier((31, 1, 32), "href".to_string()),
                    Some((
                        AttrSelectorOp::Exactly((35, 1, 36)),
                        Token::Str((36, 1, 37), "link".to_string()),
                    )),
                    true,
                )),
                Selector::PseudoElement(PseudoElementSelector::After((44, 1, 45))),
            ]))
        );
        assert_eq!(parser.pos(), (51, 1, 52));
    }

    #[test]
    fn test_parse_selector_seq7() {
        let mut parser = SelectorParser::new("div#id.cl1.cl2:nth-child(even)[href='link'I]:after");
        let res = parser.parse_selector_seq();
        assert_eq!(
            res,
            Ok(Selector::Seq(vec![
                Selector::Simple(SimpleSelector::new(
                    (0, 1, 1),
                    Some(ElemType::Div),
                    Some(Token::AttrIdentifier((4, 1, 5), "id".to_string())),
                    vec![
                        Token::AttrIdentifier((7, 1, 8), "cl1".to_string()),
                        Token::AttrIdentifier((11, 1, 12), "cl2".to_string()),
                    ],
                    false,
                )),
                Selector::PseudoClass(PseudoClassSelector::NthChild(
                    (14, 1, 15),
                    NthExpr::Even((25, 1, 26)),
                )),
                Selector::Attr(AttrSelector::new(
                    (30, 1, 31),
                    Token::AttrIdentifier((31, 1, 32), "href".to_string()),
                    Some((
                        AttrSelectorOp::Exactly((35, 1, 36)),
                        Token::Str((36, 1, 37), "link".to_string()),
                    )),
                    true,
                )),
                Selector::PseudoElement(PseudoElementSelector::After((44, 1, 45))),
            ]))
        );
        assert_eq!(parser.pos(), (50, 1, 51));
    }

    #[test]
    fn test_parse_selector_seq8() {
        let mut parser = SelectorParser::new("[id]p");
        let res = parser.parse_selector_seq();
        assert_eq!(
            res,
            Ok(Selector::Attr(AttrSelector::new(
                (0, 1, 1),
                Token::AttrIdentifier((1, 1, 2), "id".to_string()),
                None,
                false
            )))
        );
        assert_eq!(parser.pos(), (4, 1, 5));
    }

    #[test]
    fn test_parse_selector_seq_single_selector1() {
        let mut parser = SelectorParser::new("[href]");
        let res = parser.parse_selector_seq();
        assert_eq!(
            res,
            Ok(Selector::Attr(AttrSelector::new(
                (0, 1, 1),
                Token::AttrIdentifier((1, 1, 2), "href".to_string()),
                None,
                false
            )))
        );
        assert_eq!(parser.pos(), (6, 1, 7));
    }

    #[test]
    fn test_parse_selector_seq_single_selector2() {
        let mut parser = SelectorParser::new(":hover");
        let res = parser.parse_selector_seq();
        assert_eq!(
            res,
            Ok(Selector::PseudoClass(PseudoClassSelector::Hover((0, 1, 1))))
        );
        assert_eq!(parser.pos(), (6, 1, 7));
    }

    #[test]
    fn test_parse_pcs_lang1() {
        let mut parser = SelectorParser::new(":lang( en )");
        let res = parser.parse_pseudo_class_selector();
        assert_eq!(
            res,
            Ok(Selector::PseudoClass(PseudoClassSelector::Lang(
                (0, 1, 1),
                Token::ElemIdentifier((7, 1, 8), "en".to_string())
            )))
        );
        assert_eq!(parser.pos(), (11, 1, 12));
    }

    #[test]
    fn test_parse_pcs_lang2() {
        let mut parser = SelectorParser::new(":lang(zh-Hans)");
        let res = parser.parse_pseudo_class_selector();
        assert_eq!(
            res,
            Ok(Selector::PseudoClass(PseudoClassSelector::Lang(
                (0, 1, 1),
                Token::ElemIdentifier((6, 1, 7), "zh-Hans".to_string())
            )))
        );
        assert_eq!(parser.pos(), (14, 1, 15));
    }

    #[test]
    fn test_parse_pcs_lang_fail() {
        let mut parser = SelectorParser::new(":lang(\"en\")");
        let res = parser.parse_pseudo_class_selector();
        assert_eq!(
            res,
            Err(SelectorParserError::Unexpected(
                (6, 1, 7),
                "expected element identifier".to_string()
            ))
        );
        assert_eq!(parser.pos(), (6, 1, 7));
    }

    #[test]
    fn test_parse_pcs_not1() {
        let mut parser = SelectorParser::new(":not( a )");
        let res = parser.parse_pseudo_class_selector();
        assert_eq!(
            res,
            Ok(Selector::PseudoClass(PseudoClassSelector::Not(
                (0, 1, 1),
                Box::new(Selector::Simple(SimpleSelector::new(
                    (6, 1, 7),
                    Some(ElemType::A),
                    None,
                    vec![],
                    false,
                )))
            )))
        );
        assert_eq!(parser.pos(), (9, 1, 10));
    }

    #[test]
    fn test_parse_pcs_not2() {
        let mut parser = SelectorParser::new(":not( a , [href] )");
        let res = parser.parse_pseudo_class_selector();
        assert_eq!(
            res,
            Ok(Selector::PseudoClass(PseudoClassSelector::Not(
                (0, 1, 1),
                Box::new(Selector::Group(vec![
                    Selector::Simple(SimpleSelector::new(
                        (6, 1, 7),
                        Some(ElemType::A),
                        None,
                        vec![],
                        false,
                    )),
                    Selector::Attr(AttrSelector::new(
                        (10, 1, 11),
                        Token::AttrIdentifier((11, 1, 12), "href".to_string()),
                        None,
                        false,
                    )),
                ]))
            )))
        );
        assert_eq!(parser.pos(), (18, 1, 19));
    }

    #[test]
    fn test_parse_pcs_not3() {
        let mut parser = SelectorParser::new(":not( a [href] )");
        let res = parser.parse_pseudo_class_selector();
        assert_eq!(
            res,
            Ok(Selector::PseudoClass(PseudoClassSelector::Not(
                (0, 1, 1),
                Box::new(Selector::Combinator(
                    Box::new(Selector::Simple(SimpleSelector::new(
                        (6, 1, 7),
                        Some(ElemType::A),
                        None,
                        vec![],
                        false
                    ))),
                    Combinator::Descendant((7, 1, 8)),
                    Box::new(Selector::Attr(AttrSelector::new(
                        (8, 1, 9),
                        Token::AttrIdentifier((9, 1, 10), "href".to_string()),
                        None,
                        false,
                    )))
                ))
            )))
        );
        assert_eq!(parser.pos(), (16, 1, 17));
    }

    #[test]
    fn test_parse_pcs_matches1() {
        let mut parser = SelectorParser::new(":matches(a, #id, .cl)");
        let res = parser.parse_pseudo_class_selector();
        assert_eq!(
            res,
            Ok(Selector::PseudoClass(PseudoClassSelector::Matches(
                (0, 1, 1),
                Box::new(Selector::Group(vec![
                    Selector::Simple(SimpleSelector::new(
                        (9, 1, 10),
                        Some(ElemType::A),
                        None,
                        vec![],
                        false,
                    )),
                    Selector::Simple(SimpleSelector::new(
                        (12, 1, 13),
                        None,
                        Some(Token::AttrIdentifier((13, 1, 14), "id".to_string())),
                        vec![],
                        false,
                    )),
                    Selector::Simple(SimpleSelector::new(
                        (17, 1, 18),
                        None,
                        None,
                        vec![Token::AttrIdentifier((18, 1, 19), "cl".to_string())],
                        false,
                    )),
                ]))
            )))
        );
        assert_eq!(parser.pos(), (21, 1, 22));
    }

    #[test]
    fn test_parse_pcs_not_fail2() {
        let mut parser = SelectorParser::new(":not( a , [href]");
        let res = parser.parse_pseudo_class_selector();
        assert_eq!(res, Err(SelectorParserError::Eof((16, 1, 17))));
        assert_eq!(parser.pos(), (16, 1, 17));
    }

    #[test]
    fn test_parse_nth_expr1() {
        let mut parser = SelectorParser::new(" 12");
        let res = parser.parse_nth_expr();
        assert_eq!(res, Ok(NthExpr::A((1, 1, 2), Token::Number((1, 1, 2), 12))));
        assert_eq!(parser.pos(), (3, 1, 4));
    }

    #[test]
    fn test_parse_nth_expr2() {
        let mut parser = SelectorParser::new(" 12n");
        let res = parser.parse_nth_expr();
        assert_eq!(
            res,
            Ok(NthExpr::AnOpB(
                (1, 1, 2),
                Some(Token::Number((1, 1, 2), 12)),
                None,
                None
            ))
        );
        assert_eq!(parser.pos(), (4, 1, 5));
    }

    #[test]
    fn test_parse_nth_expr3() {
        let mut parser = SelectorParser::new(" 12n-3");
        let res = parser.parse_nth_expr();
        assert_eq!(
            res,
            Ok(NthExpr::AnOpB(
                (1, 1, 2),
                Some(Token::Number((1, 1, 2), 12)),
                Some(NthExprOp::Sub((4, 1, 5))),
                Some(Token::Number((5, 1, 6), 3))
            ))
        );
        assert_eq!(parser.pos(), (6, 1, 7));
    }

    #[test]
    fn test_parse_nth_expr4() {
        let mut parser = SelectorParser::new("12n + 3");
        let res = parser.parse_nth_expr();
        assert_eq!(
            res,
            Ok(NthExpr::AnOpB(
                (0, 1, 1),
                Some(Token::Number((0, 1, 1), 12)),
                Some(NthExprOp::Add((4, 1, 5))),
                Some(Token::Number((6, 1, 7), 3))
            ))
        );
        assert_eq!(parser.pos(), (7, 1, 8));
    }

    #[test]
    fn test_parse_nth_expr_even() {
        let mut parser = SelectorParser::new("even");
        let res = parser.parse_nth_expr();
        assert_eq!(res, Ok(NthExpr::Even((0, 1, 1))));
        assert_eq!(parser.pos(), (4, 1, 5));
    }

    #[test]
    fn test_parse_nth_expr_odd() {
        let mut parser = SelectorParser::new("odd");
        let res = parser.parse_nth_expr();
        assert_eq!(res, Ok(NthExpr::Odd((0, 1, 1))));
        assert_eq!(parser.pos(), (3, 1, 4));
    }

    #[test]
    fn test_parse_nth_expr5() {
        let mut parser = SelectorParser::new("n");
        let res = parser.parse_nth_expr();
        assert_eq!(res, Ok(NthExpr::AnOpB((0, 1, 1), None, None, None)));
        assert_eq!(parser.pos(), (1, 1, 2));
    }

    #[test]
    fn test_parse_nth_expr6() {
        let mut parser = SelectorParser::new("-n");
        let res = parser.parse_nth_expr();
        assert_eq!(
            res,
            Ok(NthExpr::AnOpB(
                (0, 1, 1),
                Some(Token::Number((0, 1, 1), -1)),
                None,
                None
            ))
        );
        assert_eq!(parser.pos(), (2, 1, 3));
    }

    #[test]
    fn test_parse_nth_expr7() {
        let mut parser = SelectorParser::new("-1n");
        let res = parser.parse_nth_expr();
        assert_eq!(
            res,
            Ok(NthExpr::AnOpB(
                (0, 1, 1),
                Some(Token::Number((0, 1, 1), -1)),
                None,
                None
            ))
        );
        assert_eq!(parser.pos(), (3, 1, 4));
    }

    #[test]
    fn test_parse_nth_expr8() {
        let mut parser = SelectorParser::new("-1n -3");
        let res = parser.parse_nth_expr();
        assert_eq!(
            res,
            Ok(NthExpr::AnOpB(
                (0, 1, 1),
                Some(Token::Number((0, 1, 1), -1)),
                Some(NthExprOp::Sub((4, 1, 5))),
                Some(Token::Number((5, 1, 6), 3))
            ))
        );
        assert_eq!(parser.pos(), (6, 1, 7));
    }

    #[test]
    fn test_parse_nth_expr9() {
        let mut parser = SelectorParser::new("-1");
        let res = parser.parse_nth_expr();
        assert_eq!(res, Ok(NthExpr::A((0, 1, 1), Token::Number((0, 1, 1), -1))));
        assert_eq!(parser.pos(), (2, 1, 3));
    }

    #[test]
    fn test_parse_nth_expr10() {
        let mut parser = SelectorParser::new("+3");
        let res = parser.parse_nth_expr();
        assert_eq!(res, Ok(NthExpr::A((0, 1, 1), Token::Number((0, 1, 1), 3))));
        assert_eq!(parser.pos(), (2, 1, 3));
    }

    #[test]
    fn test_parse_nth_expr11() {
        let mut parser = SelectorParser::new("-0n+1");
        let res = parser.parse_nth_expr();
        assert_eq!(
            res,
            Ok(NthExpr::AnOpB(
                (0, 1, 1),
                Some(Token::Number((0, 1, 1), 0)),
                Some(NthExprOp::Add((3, 1, 4))),
                Some(Token::Number((4, 1, 5), 1))
            ))
        );
        assert_eq!(parser.pos(), (5, 1, 6));
    }

    #[test]
    fn test_parse_nth_expr_fail1() {
        let mut parser = SelectorParser::new("- n");
        let res = parser.parse_nth_expr();
        assert_eq!(
            res,
            Err(SelectorParserError::Unexpected(
                (0, 1, 1),
                "expected an + b expression, even, or odd".to_string()
            ))
        );
        assert_eq!(parser.pos(), (2, 1, 3));
    }

    #[test]
    fn test_parse_nth_expr_fail2() {
        let mut parser = SelectorParser::new("-1 -3");
        let res = parser.parse_nth_expr();
        assert_eq!(
            res,
            Err(SelectorParserError::Unexpected(
                (0, 1, 1),
                "expected an + b expression, even, or odd".to_string()
            ))
        );
        assert_eq!(parser.pos(), (5, 1, 6));
    }

    #[test]
    fn test_parse_nth_expr_fail3() {
        let mut parser = SelectorParser::new("--3");
        let res = parser.parse_nth_expr();
        assert_eq!(
            res,
            Err(SelectorParserError::Unexpected(
                (0, 1, 1),
                "expected an + b expression, even, or odd".to_string()
            ))
        );
        assert_eq!(parser.pos(), (3, 1, 4));
    }

    #[test]
    fn test_parse_nth_expr_fail4() {
        let mut parser = SelectorParser::new("- -3");
        let res = parser.parse_nth_expr();
        assert_eq!(
            res,
            Err(SelectorParserError::Unexpected(
                (0, 1, 1),
                "expected an + b expression, even, or odd".to_string()
            ))
        );
        assert_eq!(parser.pos(), (4, 1, 5));
    }

    #[test]
    fn test_parse_pcs_nth_child1() {
        let mut parser = SelectorParser::new(":nth-child(even)");
        let res = parser.parse_pseudo_class_selector();
        assert_eq!(
            res,
            Ok(Selector::PseudoClass(PseudoClassSelector::NthChild(
                (0, 1, 1),
                NthExpr::Even((11, 1, 12))
            )))
        );
        assert_eq!(parser.pos(), (16, 1, 17));
    }

    #[test]
    fn test_parse_pcs_nth_child2() {
        let mut parser = SelectorParser::new(":nth-child( 2n )");
        let res = parser.parse_pseudo_class_selector();
        assert_eq!(
            res,
            Ok(Selector::PseudoClass(PseudoClassSelector::NthChild(
                (0, 1, 1),
                NthExpr::AnOpB((12, 1, 13), Some(Token::Number((12, 1, 13), 2)), None, None)
            )))
        );
        assert_eq!(parser.pos(), (16, 1, 17));
    }

    #[test]
    fn test_parse_pcs_nth_child3() {
        let mut parser = SelectorParser::new(":nth-child(2n-3)");
        let res = parser.parse_pseudo_class_selector();
        assert_eq!(
            res,
            Ok(Selector::PseudoClass(PseudoClassSelector::NthChild(
                (0, 1, 1),
                NthExpr::AnOpB(
                    (11, 1, 12),
                    Some(Token::Number((11, 1, 12), 2)),
                    Some(NthExprOp::Sub((13, 1, 14))),
                    Some(Token::Number((14, 1, 15), 3)),
                )
            )))
        );
        assert_eq!(parser.pos(), (16, 1, 17));
    }

    #[test]
    fn test_parse_pcs_nth_child_fail1() {
        let mut parser = SelectorParser::new(":nth-child(2 n-3)");
        let res = parser.parse_pseudo_class_selector();
        assert_eq!(
            res,
            Err(SelectorParserError::Unexpected(
                (13, 1, 14),
                "expected ')', got 'n'".to_string()
            ))
        );
        assert_eq!(parser.pos(), (13, 1, 14));
    }

    #[test]
    fn test_parse_pes1() {
        let mut parser = SelectorParser::new("::after");
        let res = parser.parse_pseudo_element_selector();
        assert_eq!(
            res,
            Ok(Selector::PseudoElement(PseudoElementSelector::After((
                0, 1, 1
            ))))
        );
        assert_eq!(parser.pos(), (7, 1, 8))
    }

    #[test]
    fn test_parse_pes2() {
        let mut parser = SelectorParser::new(":after");
        let res = parser.parse_pseudo_element_selector();
        assert_eq!(
            res,
            Ok(Selector::PseudoElement(PseudoElementSelector::After((
                0, 1, 1
            ))))
        );
        assert_eq!(parser.pos(), (6, 1, 7))
    }

    #[test]
    fn test_parse_pes3() {
        let mut parser = SelectorParser::new("::first-letter");
        let res = parser.parse_pseudo_element_selector();
        assert_eq!(
            res,
            Ok(Selector::PseudoElement(PseudoElementSelector::FirstLetter(
                (0, 1, 1)
            )))
        );
        assert_eq!(parser.pos(), (14, 1, 15))
    }

    #[test]
    fn test_parse_pes4() {
        let mut parser = SelectorParser::new(":first-letter");
        let res = parser.parse_pseudo_element_selector();
        assert_eq!(
            res,
            Ok(Selector::PseudoElement(PseudoElementSelector::FirstLetter(
                (0, 1, 1)
            )))
        );
        assert_eq!(parser.pos(), (13, 1, 14))
    }

    #[test]
    fn test_parse_pes_fail1() {
        let mut parser = SelectorParser::new("::first-lette");
        let res = parser.parse_pseudo_element_selector();
        assert_eq!(
            res,
            Err(SelectorParserError::Unexpected(
                (0, 1, 1),
                "unsupported pseudo-element selector: ::first-lette".to_string()
            ))
        );
        assert_eq!(parser.pos(), (13, 1, 14))
    }

    #[test]
    fn test_parse_combinator1() {
        let mut parser = SelectorParser::new(" +");
        let res = parser.parse_combinator();
        assert_eq!(res, Ok(Combinator::AdjacentSibling((1, 1, 2))));
        assert_eq!(parser.pos(), (2, 1, 3));
    }

    #[test]
    fn test_parse_combinator2() {
        let mut parser = SelectorParser::new(" a");
        let res = parser.parse_combinator();
        assert_eq!(res, Ok(Combinator::Descendant((0, 1, 1))));
        assert_eq!(parser.pos(), (1, 1, 2));
    }

    #[test]
    fn test_parse_selector1() {
        let mut parser = SelectorParser::new("div#id.cl1.cl2:nth-child(even)[href='link'I]::after");
        let res = parser.parse_selector();
        assert_eq!(
            res,
            Ok(Selector::Seq(vec![
                Selector::Simple(SimpleSelector::new(
                    (0, 1, 1),
                    Some(ElemType::Div),
                    Some(Token::AttrIdentifier((4, 1, 5), "id".to_string())),
                    vec![
                        Token::AttrIdentifier((7, 1, 8), "cl1".to_string()),
                        Token::AttrIdentifier((11, 1, 12), "cl2".to_string()),
                    ],
                    false,
                )),
                Selector::PseudoClass(PseudoClassSelector::NthChild(
                    (14, 1, 15),
                    NthExpr::Even((25, 1, 26)),
                )),
                Selector::Attr(AttrSelector::new(
                    (30, 1, 31),
                    Token::AttrIdentifier((31, 1, 32), "href".to_string()),
                    Some((
                        AttrSelectorOp::Exactly((35, 1, 36)),
                        Token::Str((36, 1, 37), "link".to_string()),
                    )),
                    true,
                )),
                Selector::PseudoElement(PseudoElementSelector::After((44, 1, 45))),
            ]))
        );
        assert_eq!(parser.pos(), (51, 1, 52));
    }

    #[test]
    fn test_parse_selector2() {
        let mut parser = SelectorParser::new("div   div");
        let res = parser.parse_selector();
        assert_eq!(
            res,
            Ok(Selector::Combinator(
                Box::new(Selector::Simple(SimpleSelector::new(
                    (0, 1, 1),
                    Some(ElemType::Div),
                    None,
                    vec![],
                    false,
                ))),
                Combinator::Descendant((3, 1, 4)),
                Box::new(Selector::Simple(SimpleSelector::new(
                    (6, 1, 7),
                    Some(ElemType::Div),
                    None,
                    vec![],
                    false,
                )))
            ))
        );
        assert_eq!(parser.pos(), (9, 1, 10));
    }

    #[test]
    fn test_parse_selector3() {
        let mut parser = SelectorParser::new("div  >div");
        let res = parser.parse_selector();
        assert_eq!(
            res,
            Ok(Selector::Combinator(
                Box::new(Selector::Simple(SimpleSelector::new(
                    (0, 1, 1),
                    Some(ElemType::Div),
                    None,
                    vec![],
                    false,
                ))),
                Combinator::Child((5, 1, 6)),
                Box::new(Selector::Simple(SimpleSelector::new(
                    (6, 1, 7),
                    Some(ElemType::Div),
                    None,
                    vec![],
                    false,
                )))
            ))
        );
        assert_eq!(parser.pos(), (9, 1, 10));
    }

    #[test]
    fn test_parse_selector4() {
        let mut parser = SelectorParser::new("div > div + div");
        let res = parser.parse_selector();
        assert_eq!(
            res,
            Ok(Selector::Combinator(
                Box::new(Selector::Simple(SimpleSelector::new(
                    (0, 1, 1),
                    Some(ElemType::Div),
                    None,
                    vec![],
                    false,
                ))),
                Combinator::Child((4, 1, 5)),
                Box::new(Selector::Combinator(
                    Box::new(Selector::Simple(SimpleSelector::new(
                        (6, 1, 7),
                        Some(ElemType::Div),
                        None,
                        vec![],
                        false,
                    ))),
                    Combinator::AdjacentSibling((10, 1, 11)),
                    Box::new(Selector::Simple(SimpleSelector::new(
                        (12, 1, 13),
                        Some(ElemType::Div),
                        None,
                        vec![],
                        false,
                    ))),
                ))
            ))
        );
        assert_eq!(parser.pos(), (15, 1, 16));
    }

    #[test]
    fn test_parse_selector_newline1() {
        let mut parser = SelectorParser::new("div\ndiv");
        let res = parser.parse_selector();
        assert_eq!(
            res,
            Ok(Selector::Combinator(
                Box::new(Selector::Simple(SimpleSelector::new(
                    (0, 1, 1),
                    Some(ElemType::Div),
                    None,
                    vec![],
                    false,
                ))),
                Combinator::Descendant((3, 1, 4)),
                Box::new(Selector::Simple(SimpleSelector::new(
                    (4, 2, 1),
                    Some(ElemType::Div),
                    None,
                    vec![],
                    false,
                )))
            ))
        );
        assert_eq!(parser.pos(), (7, 2, 4));
    }

    #[test]
    fn test_parse_selector_newline2() {
        let mut parser = SelectorParser::new("div \ndiv");
        let res = parser.parse_selector();
        assert_eq!(
            res,
            Ok(Selector::Combinator(
                Box::new(Selector::Simple(SimpleSelector::new(
                    (0, 1, 1),
                    Some(ElemType::Div),
                    None,
                    vec![],
                    false,
                ))),
                Combinator::Descendant((3, 1, 4)),
                Box::new(Selector::Simple(SimpleSelector::new(
                    (5, 2, 1),
                    Some(ElemType::Div),
                    None,
                    vec![],
                    false,
                )))
            ))
        );
        assert_eq!(parser.pos(), (8, 2, 4));
    }

    #[test]
    fn test_parse_selector_list1() {
        let mut parser = SelectorParser::new("div   div");
        let res = parser.parse_selector_list();
        assert_eq!(
            res,
            Ok(Selector::Combinator(
                Box::new(Selector::Simple(SimpleSelector::new(
                    (0, 1, 1),
                    Some(ElemType::Div),
                    None,
                    vec![],
                    false,
                ))),
                Combinator::Descendant((3, 1, 4)),
                Box::new(Selector::Simple(SimpleSelector::new(
                    (6, 1, 7),
                    Some(ElemType::Div),
                    None,
                    vec![],
                    false,
                )))
            ))
        );
        assert_eq!(parser.pos(), (9, 1, 10));
    }

    #[test]
    fn test_parse_selector_list2() {
        let mut parser = SelectorParser::new("div , div");
        let res = parser.parse_selector_list();
        assert_eq!(
            res,
            Ok(Selector::Group(vec![
                Selector::Simple(SimpleSelector::new(
                    (0, 1, 1),
                    Some(ElemType::Div),
                    None,
                    vec![],
                    false,
                )),
                Selector::Simple(SimpleSelector::new(
                    (6, 1, 7),
                    Some(ElemType::Div),
                    None,
                    vec![],
                    false,
                )),
            ]))
        );
        assert_eq!(parser.pos(), (9, 1, 10));
    }

    #[test]
    fn test_parse_selector_list3() {
        let mut parser = SelectorParser::new("div , div,*");
        let res = parser.parse_selector_list();
        assert_eq!(
            res,
            Ok(Selector::Group(vec![
                Selector::Simple(SimpleSelector::new(
                    (0, 1, 1),
                    Some(ElemType::Div),
                    None,
                    vec![],
                    false,
                )),
                Selector::Simple(SimpleSelector::new(
                    (6, 1, 7),
                    Some(ElemType::Div),
                    None,
                    vec![],
                    false,
                )),
                Selector::Simple(SimpleSelector::new((10, 1, 11), None, None, vec![], true)),
            ]))
        );
        assert_eq!(parser.pos(), (11, 1, 12));
    }

    #[test]
    fn test_parse1() {
        let res = SelectorParser::parse("div , div", (0, 1, 1));
        assert_eq!(
            res,
            Ok(Selector::Group(vec![
                Selector::Simple(SimpleSelector::new(
                    (0, 1, 1),
                    Some(ElemType::Div),
                    None,
                    vec![],
                    false,
                )),
                Selector::Simple(SimpleSelector::new(
                    (6, 1, 7),
                    Some(ElemType::Div),
                    None,
                    vec![],
                    false,
                )),
            ]))
        );
    }

    #[test]
    fn test_parse2() {
        let res = SelectorParser::parse(":hover", (0, 1, 1));
        assert_eq!(
            res,
            Ok(Selector::PseudoClass(PseudoClassSelector::Hover((0, 1, 1))))
        );
    }

    #[test]
    fn test_parse_fail1() {
        let res = SelectorParser::parse("div , div,", (0, 1, 1));
        assert_eq!(res, Err(Error::Eof((10, 1, 11))));
    }

    #[test]
    fn test_parse_fail2() {
        let res = SelectorParser::parse("div , div ;", (0, 1, 1));
        assert_eq!(
            res,
            Err(Error::Unexpected(
                (10, 1, 11),
                "unexpected char ';' at the end of selector".to_string()
            ))
        );
    }
}
