use magicparser::htmlparser::DomNode as PDomNode;
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
