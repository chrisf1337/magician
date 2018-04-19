use magicparser::{AttrSelector, AttrSelectorOp, DomNode, Selector, SimpleSelector};
use std::collections::HashSet;

fn matches_simple_selector(
    DomNode {
        id: dom_node_id,
        elem_type: dom_node_elem_type,
        classes: dom_node_classes,
        ..
    }: &DomNode,
    SimpleSelector {
        elem_type,
        id,
        classes,
        ..
    }: &SimpleSelector,
) -> bool {
    if let Some(ref elem_type) = elem_type {
        if *elem_type != *dom_node_elem_type {
            return false;
        }
    }
    if let (Some(ref id), Some(ref dom_node_id)) = (id, dom_node_id) {
        if *id != *dom_node_id {
            return false;
        }
    }
    if !classes.is_empty() && !classes.is_subset(dom_node_classes) {
        return false;
    }
    true
}

fn matches_attr_selector(
    DomNode { attrs, .. }: &DomNode,
    AttrSelector {
        attr,
        op_val,
        case_insensitive,
    }: &AttrSelector,
) -> bool {
    match op_val {
        Some((op, val)) => {
            // Value of attr in DOM node
            let attr_value = if let Some(&Some(ref v)) = attrs.get(attr) {
                v
            } else {
                return false;
            };
            match op {
                AttrSelectorOp::Exactly => {
                    if *case_insensitive {
                        attr_value.to_lowercase() == val.to_lowercase()
                    } else {
                        attr_value == val
                    }
                }
                AttrSelectorOp::ExactlyOne => {
                    if *case_insensitive {
                        let words = attr_value
                            .split_whitespace()
                            .map(|s| s.to_lowercase())
                            .collect::<HashSet<_>>();
                        words.contains(&val.to_lowercase())
                    } else {
                        let words = attr_value
                            .split_whitespace()
                            .map(|s| s.to_string())
                            .collect::<HashSet<_>>();
                        words.contains(val)
                    }
                }
                AttrSelectorOp::ExactlyOrHyphen => {
                    if *case_insensitive {
                        attr_value
                            .split_whitespace()
                            .find(|&s| {
                                s.to_lowercase() == val.to_lowercase()
                                    || s.to_lowercase()
                                        .starts_with(&format!("{}-", val.to_lowercase()))
                            })
                            .is_some()
                    } else {
                        attr_value
                            .split_whitespace()
                            .find(|&s| s == val || s.starts_with(&format!("{}-", val)))
                            .is_some()
                    }
                }
                AttrSelectorOp::Prefixed => {
                    if *case_insensitive {
                        attr_value.to_lowercase().starts_with(&val.to_lowercase())
                    } else {
                        attr_value.starts_with(val)
                    }
                }
                AttrSelectorOp::Suffixed => {
                    if *case_insensitive {
                        attr_value.to_lowercase().ends_with(&val.to_lowercase())
                    } else {
                        attr_value.ends_with(val)
                    }
                }
                AttrSelectorOp::ContainsAtLeastOne => {
                    if *case_insensitive {
                        attr_value.to_lowercase().contains(&val.to_lowercase())
                    } else {
                        attr_value.contains(val)
                    }
                }
            }
        }
        None => match attrs.get(attr) {
            Some(_) => true,
            None => false,
        },
    }
}

fn matches(dom_node: &DomNode, selector: &Selector) -> bool {
    match selector {
        Selector::Simple(ref simple_sel) => matches_simple_selector(dom_node, simple_sel),
        Selector::Attr(ref attr_sel) => matches_attr_selector(dom_node, attr_sel),
        _ => unimplemented!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use magicparser::ElemType;

    #[test]
    fn test_matches_simple_selector1() {
        let dom_node = DomNode::new(ElemType::A, None, hashset!{}, hashmap!{}, vec![]);
        let selector = SimpleSelector::new(Some(ElemType::A), None, hashset!{}, false);
        assert!(matches_simple_selector(&dom_node, &selector));
    }

    #[test]
    fn test_matches_simple_selector_universal() {
        let dom_node = DomNode::new(
            ElemType::A,
            Some("id".to_string()),
            hashset!{"cl1".to_string()},
            hashmap!{
                "id".to_string() => Some("id".to_string()),
                "class".to_string() => Some("cl1".to_string()),
            },
            vec![],
        );
        let selector = SimpleSelector::new(None, None, hashset!{}, true);
        assert!(matches_simple_selector(&dom_node, &selector));
    }

    #[test]
    fn test_matches_simple_selector2() {
        let dom_node = DomNode::new(
            ElemType::A,
            Some("id".to_string()),
            hashset!{},
            hashmap!{
                "id".to_string() => Some("id".to_string())
            },
            vec![],
        );
        let selector = SimpleSelector::new(None, Some("id".to_string()), hashset!{}, false);
        assert!(matches_simple_selector(&dom_node, &selector));
    }

    #[test]
    fn test_matches_simple_selector3() {
        let dom_node = DomNode::new(
            ElemType::A,
            None,
            hashset!{"cl1".to_string(), "cl2".to_string()},
            hashmap!{},
            vec![],
        );
        let selector = SimpleSelector::new(None, None, hashset!{"cl2".to_string()}, false);
        assert!(matches_simple_selector(&dom_node, &selector));
    }

    #[test]
    fn test_matches_simple_selector_fail1() {
        let dom_node = DomNode::new(
            ElemType::A,
            Some("id".to_string()),
            hashset!{"cl1".to_string()},
            hashmap!{},
            vec![],
        );
        let selector = SimpleSelector::new(
            Some(ElemType::P),
            Some("id".to_string()),
            hashset!{"cl1".to_string(), "cl2".to_string()},
            true,
        );
        assert!(!matches_simple_selector(&dom_node, &selector));
    }

    #[test]
    fn test_matches_simple_selector_fail2() {
        let dom_node = DomNode::new(
            ElemType::A,
            Some("id".to_string()),
            hashset!{},
            hashmap!{},
            vec![],
        );
        let selector =
            SimpleSelector::new(Some(ElemType::P), Some("id".to_string()), hashset!{}, false);
        assert!(!matches_simple_selector(&dom_node, &selector));
    }

    #[test]
    fn test_matches_simple_selector_fail3() {
        let dom_node = DomNode::new(
            ElemType::A,
            Some("id1".to_string()),
            hashset!{},
            hashmap!{},
            vec![],
        );
        let selector = SimpleSelector::new(None, Some("id2".to_string()), hashset!{}, false);
        assert!(!matches_simple_selector(&dom_node, &selector));
    }

    #[test]
    fn test_matches_attr_selector_no_op_val() {
        let dom_node = DomNode::new(
            ElemType::A,
            Some("id1".to_string()),
            hashset!{},
            hashmap!{
                "id".to_string() => Some("id1".to_string())
            },
            vec![],
        );
        let selector = AttrSelector::new("id".to_string(), None, false);
        assert!(matches_attr_selector(&dom_node, &selector));
    }

    #[test]
    fn test_matches_attr_selector_no_op_val_fail() {
        let dom_node = DomNode::new(
            ElemType::A,
            Some("id1".to_string()),
            hashset!{},
            hashmap!{
                "id".to_string() => Some("id1".to_string())
            },
            vec![],
        );
        let selector = AttrSelector::new("attr".to_string(), None, false);
        assert!(!matches_attr_selector(&dom_node, &selector));
    }

    #[test]
    fn test_matches_attr_selector_exactly() {
        let dom_node = DomNode::new(
            ElemType::A,
            Some("id1".to_string()),
            hashset!{},
            hashmap!{
                "id".to_string() => Some("id1".to_string())
            },
            vec![],
        );
        let selector = AttrSelector::new(
            "id".to_string(),
            Some((AttrSelectorOp::Exactly, "id1".to_string())),
            false,
        );
        assert!(matches_attr_selector(&dom_node, &selector));
    }

    #[test]
    fn test_matches_attr_selector_exactly_fail() {
        let dom_node = DomNode::new(
            ElemType::A,
            Some("id1".to_string()),
            hashset!{},
            hashmap!{
                "id".to_string() => Some("id1".to_string())
            },
            vec![],
        );
        let selector = AttrSelector::new(
            "id".to_string(),
            Some((AttrSelectorOp::Exactly, "Id1".to_string())),
            false,
        );
        assert!(!matches_attr_selector(&dom_node, &selector));
    }

    #[test]
    fn test_matches_attr_selector_exactly_case_insensitive() {
        let dom_node = DomNode::new(
            ElemType::A,
            Some("id1".to_string()),
            hashset!{},
            hashmap!{
                "id".to_string() => Some("iD1".to_string())
            },
            vec![],
        );
        let selector = AttrSelector::new(
            "id".to_string(),
            Some((AttrSelectorOp::Exactly, "Id1".to_string())),
            true,
        );
        assert!(matches_attr_selector(&dom_node, &selector));
    }

    #[test]
    fn test_matches_attr_selector_exactly_one() {
        let dom_node = DomNode::new(
            ElemType::A,
            None,
            hashset!{},
            hashmap!{
                "attr".to_string() => Some("val1 val2 val3".to_string())
            },
            vec![],
        );
        let selector = AttrSelector::new(
            "attr".to_string(),
            Some((AttrSelectorOp::ExactlyOne, "val2".to_string())),
            false,
        );
        assert!(matches_attr_selector(&dom_node, &selector));
    }

    #[test]
    fn test_matches_attr_selector_exactly_one_fail() {
        let dom_node = DomNode::new(
            ElemType::A,
            None,
            hashset!{},
            hashmap!{
                "attr".to_string() => Some("val1 val2 val3".to_string())
            },
            vec![],
        );
        let selector = AttrSelector::new(
            "attr".to_string(),
            Some((AttrSelectorOp::ExactlyOne, "val".to_string())),
            false,
        );
        assert!(!matches_attr_selector(&dom_node, &selector));
    }

    #[test]
    fn test_matches_attr_selector_exactly_one_case_insensitive() {
        let dom_node = DomNode::new(
            ElemType::A,
            None,
            hashset!{},
            hashmap!{
                "attr".to_string() => Some("vaL1 vAl2 Val3".to_string())
            },
            vec![],
        );
        let selector = AttrSelector::new(
            "attr".to_string(),
            Some((AttrSelectorOp::ExactlyOne, "VaL2".to_string())),
            true,
        );
        assert!(matches_attr_selector(&dom_node, &selector));
    }

    #[test]
    fn test_matches_attr_selector_exactly_or_hyphen1() {
        let dom_node = DomNode::new(
            ElemType::A,
            None,
            hashset!{},
            hashmap!{
                "attr".to_string() => Some("val-1".to_string())
            },
            vec![],
        );
        let selector = AttrSelector::new(
            "attr".to_string(),
            Some((AttrSelectorOp::ExactlyOrHyphen, "val".to_string())),
            false,
        );
        assert!(matches_attr_selector(&dom_node, &selector));
    }

    #[test]
    fn test_matches_attr_selector_exactly_or_hyphen2() {
        let dom_node = DomNode::new(
            ElemType::A,
            None,
            hashset!{},
            hashmap!{
                "attr".to_string() => Some("val-1".to_string())
            },
            vec![],
        );
        let selector = AttrSelector::new(
            "attr".to_string(),
            Some((AttrSelectorOp::ExactlyOrHyphen, "val-1".to_string())),
            false,
        );
        assert!(matches_attr_selector(&dom_node, &selector));
    }

    #[test]
    fn test_matches_attr_selector_exactly_or_hyphen_fail() {
        let dom_node = DomNode::new(
            ElemType::A,
            None,
            hashset!{},
            hashmap!{
                "attr".to_string() => Some("val-1".to_string())
            },
            vec![],
        );
        let selector = AttrSelector::new(
            "attr".to_string(),
            Some((AttrSelectorOp::ExactlyOrHyphen, "val1".to_string())),
            false,
        );
        assert!(!matches_attr_selector(&dom_node, &selector));
    }

    #[test]
    fn test_matches_attr_selector_prefixed() {
        let dom_node = DomNode::new(
            ElemType::A,
            None,
            hashset!{},
            hashmap!{
                "attr".to_string() => Some("val1".to_string())
            },
            vec![],
        );
        let selector = AttrSelector::new(
            "attr".to_string(),
            Some((AttrSelectorOp::Prefixed, "va".to_string())),
            false,
        );
        assert!(matches_attr_selector(&dom_node, &selector));
    }

    #[test]
    fn test_matches_attr_selector_prefixed_fail() {
        let dom_node = DomNode::new(
            ElemType::A,
            None,
            hashset!{},
            hashmap!{
                "attr".to_string() => Some("val1".to_string())
            },
            vec![],
        );
        let selector = AttrSelector::new(
            "attr".to_string(),
            Some((AttrSelectorOp::Prefixed, "al".to_string())),
            false,
        );
        assert!(!matches_attr_selector(&dom_node, &selector));
    }

    #[test]
    fn test_matches_attr_selector_prefixed_case_insensitive() {
        let dom_node = DomNode::new(
            ElemType::A,
            None,
            hashset!{},
            hashmap!{
                "attr".to_string() => Some("vAl1".to_string())
            },
            vec![],
        );
        let selector = AttrSelector::new(
            "attr".to_string(),
            Some((AttrSelectorOp::Prefixed, "VaL".to_string())),
            true,
        );
        assert!(matches_attr_selector(&dom_node, &selector));
    }

    #[test]
    fn test_matches_attr_selector_suffixed() {
        let dom_node = DomNode::new(
            ElemType::A,
            None,
            hashset!{},
            hashmap!{
                "attr".to_string() => Some("val1".to_string())
            },
            vec![],
        );
        let selector = AttrSelector::new(
            "attr".to_string(),
            Some((AttrSelectorOp::Suffixed, "l1".to_string())),
            false,
        );
        assert!(matches_attr_selector(&dom_node, &selector));
    }

    #[test]
    fn test_matches_attr_selector_suffixed_fail() {
        let dom_node = DomNode::new(
            ElemType::A,
            None,
            hashset!{},
            hashmap!{
                "attr".to_string() => Some("val1".to_string())
            },
            vec![],
        );
        let selector = AttrSelector::new(
            "attr".to_string(),
            Some((AttrSelectorOp::Suffixed, "al".to_string())),
            false,
        );
        assert!(!matches_attr_selector(&dom_node, &selector));
    }

    #[test]
    fn test_matches_attr_selector_suffixed_case_insensitive() {
        let dom_node = DomNode::new(
            ElemType::A,
            None,
            hashset!{},
            hashmap!{
                "attr".to_string() => Some("vAl1".to_string())
            },
            vec![],
        );
        let selector = AttrSelector::new(
            "attr".to_string(),
            Some((AttrSelectorOp::Suffixed, "aL1".to_string())),
            true,
        );
        assert!(matches_attr_selector(&dom_node, &selector));
    }

    #[test]
    fn test_matches_attr_selector_contains_at_least_one() {
        let dom_node = DomNode::new(
            ElemType::A,
            None,
            hashset!{},
            hashmap!{
                "attr".to_string() => Some("http://www.example.com".to_string())
            },
            vec![],
        );
        let selector = AttrSelector::new(
            "attr".to_string(),
            Some((AttrSelectorOp::ContainsAtLeastOne, "example".to_string())),
            false,
        );
        assert!(matches_attr_selector(&dom_node, &selector));
    }

    #[test]
    fn test_matches_attr_selector_contains_at_least_one_fail() {
        let dom_node = DomNode::new(
            ElemType::A,
            None,
            hashset!{},
            hashmap!{
                "attr".to_string() => Some("http://www.example.com".to_string())
            },
            vec![],
        );
        let selector = AttrSelector::new(
            "attr".to_string(),
            Some((AttrSelectorOp::ContainsAtLeastOne, "notexample".to_string())),
            false,
        );
        assert!(!matches_attr_selector(&dom_node, &selector));
    }

    #[test]
    fn test_matches_attr_selector_contains_at_least_one_case_insensitive() {
        let dom_node = DomNode::new(
            ElemType::A,
            None,
            hashset!{},
            hashmap!{
                "attr".to_string() => Some("http://www.ExAmplE.com".to_string())
            },
            vec![],
        );
        let selector = AttrSelector::new(
            "attr".to_string(),
            Some((
                AttrSelectorOp::ContainsAtLeastOne,
                "exAMpLe.Com".to_string(),
            )),
            true,
        );
        assert!(matches_attr_selector(&dom_node, &selector));
    }
}
