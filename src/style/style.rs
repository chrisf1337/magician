use magicparser::DomNode;
use magicparser::selectorparser::{Selector, SimpleSelector};

fn matches_simple_selector(
    dom_node: &DomNode,
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
    match elem_type {
        &Some(ref elem_type) => if *elem_type == dom_node.elem_type {
            return true;
        } else {
            ()
        },
        None => (),
    }
    // match id {
    //     &Some(ref id) => if *id == dom_node.id { return true; } else { () },
    //     None => (),
    // }
    false
}

fn matches(dom_node: &DomNode, selector: &Selector) -> bool {
    match selector {
        &Selector::Simple(ref simple_sel) => matches_simple_selector(dom_node, simple_sel),
        _ => unimplemented!(),
    }
}