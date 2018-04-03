use magicparser::htmlparser::ParserDomNode;
use magicparser::selectorparser::{Selector, SimpleSelector};

fn matches_simple_selector(
    dom_node: &ParserDomNode,
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
    false
    // match id {
    //     &Some(ref id) => if *id == dom_node.id { return true; } else { () },
    //     None => (),
    // }
}

fn matches(dom_node: &ParserDomNode, selector: &Selector) -> bool {
    match selector {
        &Selector::Simple(ref simple_sel) => matches_simple_selector(dom_node, simple_sel),
        _ => unimplemented!(),
    }
}
