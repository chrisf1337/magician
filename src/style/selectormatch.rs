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
        universal,
        ..
    }: &SimpleSelector,
) -> bool {
    if *universal {
        return true;
    }
    if let Some(ref elem_type) = elem_type {
        if *elem_type == *dom_node_elem_type {
            return true;
        }
    }
    if let (Some(ref id), Some(ref dom_node_id)) = (id, dom_node_id) {
        if *id == *dom_node_id {
            return true;
        }
    }
    if classes.intersection(dom_node_classes).count() > 0 {
        return true;
    }
    false
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
}
