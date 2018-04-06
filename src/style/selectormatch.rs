use magicparser::{DomNode, Selector, SimpleSelector};

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

fn matches(dom_node: &DomNode, selector: &Selector) -> bool {
    match selector {
        Selector::Simple(ref simple_sel) => matches_simple_selector(dom_node, simple_sel),
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
}
