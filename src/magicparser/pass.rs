use magicparser::htmlparser::DomNode as PDomNode;
use magicparser::selectorparser::{AttrSelector as PAttrSelector,
                                  AttrSelectorOp as PAttrSelectorOp, Combinator as PCombinator,
                                  NthExpr as PNthExpr, NthExprOp as PNthExprOp,
                                  PseudoClassSelector as PPseudoClassSelector,
                                  PseudoClassSelectorType as PPseudoClassSelectorType,
                                  PseudoElementSelector as PPseudoElementSelector,
                                  Selector as PSelector, SimpleSelector as PSimpleSelector};
use magicparser::{ElemType, Token};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct DomNode {
    pub elem_type: ElemType,
    pub id: Option<String>,
    pub classes: HashSet<String>,
    pub attrs: HashMap<String, Option<String>>,
    pub children: Vec<DomNode>,
}

impl DomNode {
    fn new(
        elem_type: ElemType,
        id: Option<String>,
        classes: HashSet<String>,
        attrs: HashMap<String, Option<String>>,
        children: Vec<DomNode>,
    ) -> DomNode {
        DomNode {
            elem_type,
            id,
            classes,
            attrs,
            children,
        }
    }
}

impl From<PDomNode> for DomNode {
    fn from(
        PDomNode {
            elem_type,
            attrs,
            children,
            ..
        }: PDomNode,
    ) -> Self {
        let mut id: Option<String> = None;
        let mut classes: HashSet<String> = HashSet::new();
        let mut deduped_attrs: HashMap<String, Option<String>> = HashMap::new();
        for &(ref attr, ref val) in attrs.iter() {
            let value = match val {
                Some(Token::Value(_, ref value_str)) => Some(value_str.to_string()),
                Some(Token::Str(_, ref value_str)) => Some(value_str.to_string()),
                _ => None,
            };
            if let Token::AttrIdentifier(_, attr_str) = attr {
                if !deduped_attrs.contains_key(attr_str) {
                    deduped_attrs.insert(attr_str.to_string(), value);
                }
            }
        }
        for (&ref attr, &ref value) in deduped_attrs.iter() {
            match attr.as_ref() {
                "id" => id = value.clone(),
                "class" => if let Some(value) = value {
                    classes.extend(value.split_whitespace().map(|s| s.to_string()))
                },
                _ => (),
            }
        }
        DomNode::new(
            elem_type,
            id,
            classes,
            deduped_attrs,
            children
                .iter()
                .map(|&ref child| DomNode::from(child.clone()))
                .collect(),
        )
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum AttrSelectorOp {
    Exactly,            // =
    ExactlyOne,         // ~=
    ExactlyOrHyphen,    // |=
    Prefixed,           // ^=
    Suffixed,           // $=
    ContainsAtLeastOne, // *=
}

impl From<PAttrSelectorOp> for AttrSelectorOp {
    fn from(attr_selector_op: PAttrSelectorOp) -> Self {
        use self::PAttrSelectorOp::*;
        match attr_selector_op {
            Exactly(_) => AttrSelectorOp::Exactly,
            ExactlyOne(_) => AttrSelectorOp::ExactlyOne,
            ExactlyOrHyphen(_) => AttrSelectorOp::ExactlyOrHyphen,
            Prefixed(_) => AttrSelectorOp::Prefixed,
            Suffixed(_) => AttrSelectorOp::Suffixed,
            ContainsAtLeastOne(_) => AttrSelectorOp::ContainsAtLeastOne,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct SimpleSelector {
    pub elem_type: Option<ElemType>,
    pub id: Option<String>,
    pub classes: HashSet<String>,
    pub universal: bool,
}

impl SimpleSelector {
    pub fn new(
        elem_type: Option<ElemType>,
        id: Option<String>,
        classes: HashSet<String>,
        universal: bool,
    ) -> SimpleSelector {
        SimpleSelector {
            elem_type,
            id,
            classes,
            universal,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct AttrSelector {
    pub attr: String,
    pub op_val: Option<(AttrSelectorOp, String)>,
    pub case_insensitive: bool,
}

impl AttrSelector {
    pub fn new(
        attr: String,
        op_val: Option<(AttrSelectorOp, String)>,
        case_insensitive: bool,
    ) -> AttrSelector {
        AttrSelector {
            attr,
            op_val,
            case_insensitive,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum NthExprOp {
    Add,
    Sub,
}

impl From<PNthExprOp> for NthExprOp {
    fn from(op: PNthExprOp) -> Self {
        use self::PNthExprOp::*;
        match op {
            Add(..) => NthExprOp::Add,
            Sub(..) => NthExprOp::Sub,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum NthExpr {
    A(isize),
    AnPlusB(isize, Option<NthExprOp>, isize),
}

impl From<PNthExpr> for NthExpr {
    fn from(expr: PNthExpr) -> Self {
        use self::PNthExpr::*;
        match expr {
            A(.., tok) => NthExpr::A(tok.to_string().parse::<isize>().unwrap()),
            AnPlusB(.., Some(a), op, b) => match b {
                Some(b) => NthExpr::AnPlusB(
                    a.to_string().parse::<isize>().unwrap(),
                    op.map(NthExprOp::from),
                    b.to_string().parse::<isize>().unwrap(),
                ),
                None => NthExpr::AnPlusB(
                    a.to_string().parse::<isize>().unwrap(),
                    op.map(NthExprOp::from),
                    0,
                ),
            },
            AnPlusB(.., None, op, b) => match b {
                Some(b) => NthExpr::AnPlusB(
                    1,
                    op.map(NthExprOp::from),
                    b.to_string().parse::<isize>().unwrap(),
                ),
                None => NthExpr::AnPlusB(1, op.map(NthExprOp::from), 0),
            },
            Even(..) => NthExpr::AnPlusB(2, None, 0),
            Odd(..) => NthExpr::AnPlusB(2, Some(NthExprOp::Add), 1),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum PseudoClassSelector {
    Active,
    Hover,
    // experimental: Dir,
    // experimental: Host,
    // experimental: HostContext,
    Lang(String),
    Link,
    Matches(Box<Selector>),
    Visited,
    Not(Box<Selector>),
    NthChild(NthExpr),
    NthLastChild(NthExpr),
    NthLastOfType(NthExpr),
    NthOfType(NthExpr),
}

impl From<PPseudoClassSelector> for PseudoClassSelector {
    fn from(PPseudoClassSelector { sel_type, .. }: PPseudoClassSelector) -> Self {
        use self::PPseudoClassSelectorType::*;
        match sel_type {
            Active => PseudoClassSelector::Active,
            Hover => PseudoClassSelector::Hover,
            Lang(tok) => PseudoClassSelector::Lang(tok.to_string()),
            Link => PseudoClassSelector::Link,
            Matches(sel) => PseudoClassSelector::Matches(Box::new(Selector::from(*sel))),
            Visited => PseudoClassSelector::Visited,
            Not(sel) => PseudoClassSelector::Not(Box::new(Selector::from(*sel))),
            NthChild(nth_expr) => PseudoClassSelector::NthChild(NthExpr::from(nth_expr)),
            NthLastChild(nth_expr) => PseudoClassSelector::NthLastChild(NthExpr::from(nth_expr)),
            NthLastOfType(nth_expr) => PseudoClassSelector::NthLastOfType(NthExpr::from(nth_expr)),
            NthOfType(nth_expr) => PseudoClassSelector::NthOfType(NthExpr::from(nth_expr)),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum PseudoElementSelector {
    After,
    Before,
    Cue,
    FirstLetter,
    FirstLine,
    Selection,
    Slotted,
}

impl From<PPseudoElementSelector> for PseudoElementSelector {
    fn from(sel: PPseudoElementSelector) -> Self {
        use self::PPseudoElementSelector::*;
        match sel {
            After(..) => PseudoElementSelector::After,
            Before(..) => PseudoElementSelector::Before,
            Cue(..) => PseudoElementSelector::Cue,
            FirstLetter(..) => PseudoElementSelector::FirstLetter,
            FirstLine(..) => PseudoElementSelector::FirstLine,
            Selection(..) => PseudoElementSelector::Selection,
            Slotted(..) => PseudoElementSelector::Slotted,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Combinator {
    AdjacentSibling, // +
    GeneralSibling,  // ~
    Child,           // >
    Descendant,      // space
}

impl From<PCombinator> for Combinator {
    fn from(com: PCombinator) -> Self {
        use self::PCombinator::*;
        match com {
            AdjacentSibling(..) => Combinator::AdjacentSibling,
            GeneralSibling(..) => Combinator::GeneralSibling,
            Child(..) => Combinator::Child,
            Descendant(..) => Combinator::Descendant,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum Selector {
    Simple(SimpleSelector),
    Attr(AttrSelector),
    PseudoClass(PseudoClassSelector),
    PseudoElement(PseudoElementSelector),
    Seq(Vec<Selector>), // AND of selectors (e.g. a#id[href="www.example.com"]:visited )
    Combinator(Box<Selector>, Combinator, Box<Selector>),
    Group(Vec<Selector>), // comma-separated group
}

impl From<PSelector> for Selector {
    fn from(pselector: PSelector) -> Selector {
        use self::PSelector::*;
        match pselector {
            Simple(PSimpleSelector {
                elem_type,
                id,
                classes,
                universal,
                ..
            }) => {
                let id = id.map(|tok| tok.to_string());
                let classes = classes.iter().map(|cl| cl.to_string()).collect();
                Selector::Simple(SimpleSelector::new(elem_type, id, classes, universal))
            }
            Attr(PAttrSelector {
                attr,
                op_val,
                case_insensitive,
                ..
            }) => {
                let attr = attr.to_string();
                let op_val = op_val.map(|(op, tok)| (AttrSelectorOp::from(op), tok.to_string()));
                Selector::Attr(AttrSelector::new(attr, op_val, case_insensitive))
            }
            PseudoClass(sel) => Selector::PseudoClass(PseudoClassSelector::from(sel)),
            PseudoElement(sel) => Selector::PseudoElement(PseudoElementSelector::from(sel)),
            Seq(sels) => Selector::Seq(sels.into_iter().map(Selector::from).collect()),
            Combinator(sel1, com, sel2) => Selector::Combinator(
                Box::new(Selector::from(*sel1)),
                self::Combinator::from(com),
                Box::new(Selector::from(*sel2)),
            ),
            Group(sels) => Selector::Group(sels.into_iter().map(Selector::from).collect()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_convert_to_domnode1() {
        let parser_dom_node = PDomNode::new(
            (0, 1, 1),
            ElemType::A,
            vec![
                (
                    Token::AttrIdentifier((0, 1, 1), "id".to_string()),
                    Some(Token::Value((0, 1, 1), "a".to_string())),
                ),
                (
                    Token::AttrIdentifier((0, 1, 1), "attr".to_string()),
                    Some(Token::Value((0, 1, 1), "val".to_string())),
                ),
                (
                    Token::AttrIdentifier((0, 1, 1), "class".to_string()),
                    Some(Token::Str((0, 1, 1), "cl1 cl2".to_string())),
                ),
                (
                    Token::AttrIdentifier((0, 1, 1), "another-attr".to_string()),
                    None,
                ),
                (Token::AttrIdentifier((0, 1, 1), "class".to_string()), None),
                (
                    Token::AttrIdentifier((0, 1, 1), "id".to_string()),
                    Some(Token::Value((0, 1, 1), "b".to_string())),
                ),
            ],
            vec![
                PDomNode::new(
                    (0, 1, 1),
                    ElemType::Text("text".to_string()),
                    vec![],
                    vec![],
                ),
                PDomNode::new(
                    (0, 1, 1),
                    ElemType::Custom("custom".to_string()),
                    vec![],
                    vec![],
                ),
            ],
        );
        assert_eq!(
            DomNode::from(parser_dom_node),
            DomNode::new(
                ElemType::A,
                Some("a".to_string()),
                hashset! { "cl1".to_string(), "cl2".to_string() },
                hashmap! {
                    "id".to_string() => Some("a".to_string()),
                    "class".to_string() => Some("cl1 cl2".to_string()),
                    "attr".to_string() => Some("val".to_string()),
                    "another-attr".to_string() => None
                },
                vec![
                    DomNode::new(
                        ElemType::Text("text".to_string()),
                        None,
                        HashSet::new(),
                        HashMap::new(),
                        vec![],
                    ),
                    DomNode::new(
                        ElemType::Custom("custom".to_string()),
                        None,
                        HashSet::new(),
                        HashMap::new(),
                        vec![],
                    ),
                ]
            )
        )
    }
}
