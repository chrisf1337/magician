use magicparser::cssparser::{CssBlocks as CPCssBlocks, Token as CPToken};
use magicparser::htmlparser::DomNode as HPDomNode;
use magicparser::selectorparser::{AttrSelector as SPAttrSelector,
                                  AttrSelectorOp as SPAttrSelectorOp, Combinator as SPCombinator,
                                  NthExpr as SPNthExpr, NthExprOp as SPNthExprOp,
                                  PseudoClassSelector as SPPseudoClassSelector,
                                  PseudoElementSelector as SPPseudoElementSelector,
                                  Selector as SPSelector, SimpleSelector as SPSimpleSelector};
use magicparser::{ElemType, Token};
use std::cell::{Ref, RefCell, RefMut};
use std::collections::{HashMap, HashSet};
use std::convert::From;
use std::rc::{Rc, Weak};
use std::sync::atomic::{AtomicUsize, Ordering, ATOMIC_USIZE_INIT};

static DOM_NODE_NEXT_ID_NUM: AtomicUsize = ATOMIC_USIZE_INIT;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DomNodeRef {
    ptr: Rc<RefCell<DomNode>>,
}

#[derive(Debug, Clone)]
pub struct DomNode {
    pub id_num: usize,
    pub elem_type: ElemType,
    pub id: Option<String>,
    pub classes: HashSet<String>,
    pub attrs: HashMap<String, Option<String>>,
    pub parent: Option<Weak<RefCell<DomNode>>>,
    pub children: Vec<DomNodeRef>,
}

impl PartialEq for DomNode {
    fn eq(&self, other: &DomNode) -> bool {
        self.id_num == other.id_num && self.elem_type == other.elem_type && self.id == other.id
            && self.classes == other.classes && self.attrs == other.attrs
            && self.children == other.children
    }
}

impl Eq for DomNode {}

impl DomNode {
    pub fn new(
        elem_type: ElemType,
        id: Option<String>,
        classes: HashSet<String>,
        attrs: HashMap<String, Option<String>>,
        parent: Option<Weak<RefCell<DomNode>>>,
        children: Vec<DomNodeRef>,
    ) -> DomNode {
        let id_num = DOM_NODE_NEXT_ID_NUM.fetch_add(1, Ordering::SeqCst);
        DomNode {
            id_num,
            elem_type,
            id,
            classes,
            attrs,
            parent,
            children,
        }
    }

    pub fn to_dnref(self) -> DomNodeRef {
        DomNodeRef {
            ptr: Rc::new(RefCell::new(self)),
        }
    }
}

impl DomNodeRef {
    pub fn borrow_mut(&self) -> RefMut<DomNode> {
        self.ptr.borrow_mut()
    }

    pub fn borrow(&self) -> Ref<DomNode> {
        self.ptr.borrow()
    }

    pub fn upgrade_from_weak(weak: &Weak<RefCell<DomNode>>) -> Option<DomNodeRef> {
        weak.upgrade().map(|ptr| DomNodeRef { ptr })
    }

    pub fn add_child<'a>(&'a self, child: DomNodeRef) -> &'a Self {
        let node = &self.ptr;
        child.borrow_mut().parent = Some(Rc::downgrade(node));
        let mut node = node.borrow_mut();
        node.children.push(child);
        self
    }

    pub fn add_children<'a>(&'a self, children: Vec<DomNodeRef>) -> &'a Self {
        for child in children {
            self.add_child(child);
        }
        self
    }

    // Starts at 1
    pub fn child_index(&self) -> Option<usize> {
        let parent = &self.ptr.borrow().parent;
        if let Some(ref parent) = parent {
            if let Some(ref parent) = Self::upgrade_from_weak(parent) {
                return parent
                    .borrow()
                    .children
                    .iter()
                    .position(|child| child == self)
                    .map(|x| x + 1);
            }
        }
        None
    }

    pub fn eq_ignore_id_num(&self, other: &DomNodeRef) -> bool {
        let this = self.borrow();
        let other = other.borrow();

        this.elem_type == other.elem_type && this.id == other.id && this.classes == other.classes
            && this.attrs == other.attrs && this.children.len() == other.children.len()
            && this.children
                .iter()
                .zip(other.children.iter())
                .all(|(ch1, ch2)| ch1.eq_ignore_id_num(ch2))
    }
}

impl From<HPDomNode> for DomNodeRef {
    fn from(
        HPDomNode {
            elem_type,
            attrs,
            children,
            ..
        }: HPDomNode,
    ) -> DomNodeRef {
        let mut id: Option<String> = None;
        let mut classes: HashSet<String> = HashSet::new();
        let mut deduped_attrs: HashMap<String, Option<String>> = HashMap::new();
        for &(ref attr, ref val) in &attrs {
            let value = match val {
                Some(Token::Value(_, ref value_str)) | Some(Token::Str(_, ref value_str)) => {
                    Some(value_str.to_string())
                }
                // => Some(value_str.to_string()),
                None => None,
                _ => unreachable!(),
            };
            if let Token::AttrIdentifier(_, attr_str) = attr {
                if !deduped_attrs.contains_key(&attr_str.to_lowercase()) {
                    deduped_attrs.insert(attr_str.to_lowercase().to_string(), value);
                }
            }
        }
        for (attr, value) in &deduped_attrs {
            match attr.as_ref() {
                "id" => id = value.clone(),
                "class" => if let Some(value) = value {
                    classes.extend(value.split_whitespace().map(|s| s.to_string()))
                },
                _ => (),
            }
        }

        let node = DomNode::new(elem_type, id, classes, deduped_attrs, None, vec![]).to_dnref();

        let children: Vec<DomNodeRef> = children
            .iter()
            .map(|child| DomNodeRef::from(child.clone()))
            .collect();
        node.add_children(children);
        node
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum AttrSelectorOp {
    Exactly,            // =
    ExactlyOne,         // ~=
    ExactlyOrHyphen,    // |=
    Prefixed,           // ^=
    Suffixed,           // $=
    ContainsAtLeastOne, // *=
}

impl From<SPAttrSelectorOp> for AttrSelectorOp {
    fn from(attr_selector_op: SPAttrSelectorOp) -> Self {
        use self::SPAttrSelectorOp::*;
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

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum NthExprOp {
    Add,
    Sub,
}

impl From<SPNthExprOp> for NthExprOp {
    fn from(op: SPNthExprOp) -> Self {
        use self::SPNthExprOp::*;
        match op {
            Add(..) => NthExprOp::Add,
            Sub(..) => NthExprOp::Sub,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum NthExpr {
    A(isize),
    AnOpB(isize, Option<NthExprOp>, isize),
}

impl NthExpr {
    pub fn matches(&self, i: usize) -> bool {
        use self::NthExpr::*;
        let i = i as isize;
        match self {
            &A(a) => a == i,
            &AnOpB(a, Some(NthExprOp::Add), b) => (i - b) / a >= 0 && (i - b) % a == 0,

            &AnOpB(a, Some(NthExprOp::Sub), b) => (i + b) / a >= 0 && (i + b) % a == 0,
            &AnOpB(a, None, _) => i / a >= 0 && i % a == 0,
        }
    }
}

impl From<SPNthExpr> for NthExpr {
    fn from(expr: SPNthExpr) -> Self {
        use self::SPNthExpr::*;
        match expr {
            A(.., tok) => NthExpr::A(tok.to_string().parse::<isize>().unwrap()),
            AnOpB(.., Some(Token::Number(_, a)), op, b) => match b {
                Some(Token::Number(_, b)) => NthExpr::AnOpB(a, op.map(NthExprOp::from), b),
                None => NthExpr::AnOpB(a, op.map(NthExprOp::from), 0),
                _ => unreachable!(),
            },
            AnOpB(.., None, op, b) => match b {
                Some(Token::Number(_, b)) => NthExpr::AnOpB(1, op.map(NthExprOp::from), b),
                None => NthExpr::AnOpB(1, op.map(NthExprOp::from), 0),
                _ => unreachable!(),
            },
            AnOpB(..) => unreachable!(),
            Even(..) => NthExpr::AnOpB(2, None, 0),
            Odd(..) => NthExpr::AnOpB(2, Some(NthExprOp::Add), 1),
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

impl From<SPPseudoClassSelector> for PseudoClassSelector {
    fn from(sel: SPPseudoClassSelector) -> Self {
        use self::SPPseudoClassSelector::*;
        match sel {
            Active(_) => PseudoClassSelector::Active,
            Hover(_) => PseudoClassSelector::Hover,
            Lang(_, tok) => PseudoClassSelector::Lang(tok.to_string()),
            Link(_) => PseudoClassSelector::Link,
            Matches(_, sel) => PseudoClassSelector::Matches(Box::new(Selector::from(*sel))),
            Visited(_) => PseudoClassSelector::Visited,
            Not(_, sel) => PseudoClassSelector::Not(Box::new(Selector::from(*sel))),
            NthChild(_, nth_expr) => PseudoClassSelector::NthChild(NthExpr::from(nth_expr)),
            NthLastChild(_, nth_expr) => PseudoClassSelector::NthLastChild(NthExpr::from(nth_expr)),
            NthLastOfType(_, nth_expr) => {
                PseudoClassSelector::NthLastOfType(NthExpr::from(nth_expr))
            }
            NthOfType(_, nth_expr) => PseudoClassSelector::NthOfType(NthExpr::from(nth_expr)),
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

impl From<SPPseudoElementSelector> for PseudoElementSelector {
    fn from(sel: SPPseudoElementSelector) -> Self {
        use self::SPPseudoElementSelector::*;
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

impl From<SPCombinator> for Combinator {
    fn from(com: SPCombinator) -> Self {
        use self::SPCombinator::*;
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

impl From<SPSelector> for Selector {
    fn from(selector: SPSelector) -> Selector {
        use self::SPSelector::*;
        match selector {
            Simple(SPSimpleSelector {
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
            Attr(SPAttrSelector {
                attr,
                op_val,
                case_insensitive,
                ..
            }) => {
                let attr = attr.into_lowercase().to_string();
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

// Unfortunately in Rust HashSet doesn't impl Hash, so we can't have a key of Selector
#[derive(Debug, PartialEq, Eq)]
pub struct CssBlocks(pub Vec<(Selector, HashMap<String, String>)>);

impl From<CPCssBlocks> for CssBlocks {
    fn from(CPCssBlocks(blocks): CPCssBlocks) -> Self {
        let mut blks = vec![];
        for (selector, decl_block) in blocks {
            // Check if selector is already in blks
            let sel = Selector::from(selector);
            match blks.iter().position(
                |&(ref blks_sel, _): &(Selector, HashMap<String, String>)| *blks_sel == sel,
            ) {
                Some(index) => {
                    let (_, ref mut hmap) = &mut blks[index];
                    for (property, value) in decl_block {
                        if let (CPToken::Property(_, property), CPToken::Value(_, value)) =
                            (property, value)
                        {
                            hmap.insert(property.to_lowercase().to_string(), value);
                        }
                    }
                }
                None => {
                    let mut hmap = HashMap::new();
                    for (property, value) in decl_block {
                        if let (CPToken::Property(_, property), CPToken::Value(_, value)) =
                            (property, value)
                        {
                            hmap.insert(property.to_lowercase().to_string(), value);
                        }
                    }
                    blks.push((sel, hmap));
                }
            }
        }
        CssBlocks(blks)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_convert_to_domnode1() {
        let parser_dom_node = HPDomNode::new(
            (0, 1, 1),
            ElemType::A,
            vec![
                (
                    Token::AttrIdentifier((0, 1, 1), "id".to_string()),
                    Some(Token::Value((0, 1, 1), "a".to_string())),
                ),
                (
                    Token::AttrIdentifier((0, 1, 1), "AtTr".to_string()),
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
                (
                    Token::AttrIdentifier((0, 1, 1), "Another-Attr".to_string()),
                    Some(Token::Value((0, 1, 1), "a".to_string())),
                ),
            ],
            vec![
                HPDomNode::new(
                    (0, 1, 1),
                    ElemType::Text("text".to_string()),
                    vec![],
                    vec![],
                ),
                HPDomNode::new(
                    (0, 1, 1),
                    ElemType::Custom("custom".to_string()),
                    vec![],
                    vec![],
                ),
            ],
        );
        let node = DomNode::new(
            ElemType::A,
            Some("a".to_string()),
            hashset! { "cl1".to_string(), "cl2".to_string() },
            hashmap! {
                "id".to_string() => Some("a".to_string()),
                "class".to_string() => Some("cl1 cl2".to_string()),
                "attr".to_string() => Some("val".to_string()),
                "another-attr".to_string() => None
            },
            None,
            vec![],
        ).to_dnref();
        let child1 = DomNode::new(
            ElemType::Text("text".to_string()),
            None,
            HashSet::new(),
            HashMap::new(),
            None,
            vec![],
        ).to_dnref();
        let child2 = DomNode::new(
            ElemType::Custom("custom".to_string()),
            None,
            HashSet::new(),
            HashMap::new(),
            None,
            vec![],
        ).to_dnref();
        node.add_children(vec![child1, child2]);
        assert!(DomNodeRef::from(parser_dom_node).eq_ignore_id_num(&node));
        // assert_eq!(DomNodeRef::from(parser_dom_node), node)
    }

    #[test]
    fn test_convert_to_nth_expr1() {
        assert_eq!(
            // n
            NthExpr::from(SPNthExpr::AnOpB((0, 1, 1), None, None, None)),
            NthExpr::AnOpB(1, None, 0)
        );
    }

    #[test]
    fn test_convert_to_nth_expr2() {
        assert_eq!(
            // -n
            NthExpr::from(SPNthExpr::AnOpB(
                (0, 1, 1),
                Some(Token::Number((0, 1, 1), -1)),
                None,
                None
            )),
            NthExpr::AnOpB(-1, None, 0)
        );
    }

    #[test]
    fn test_convert_to_nth_expr3() {
        assert_eq!(
            // n + 1
            NthExpr::from(SPNthExpr::AnOpB(
                (0, 1, 1),
                None,
                Some(SPNthExprOp::Add((0, 1, 1))),
                Some(Token::Number((0, 1, 1), 1))
            )),
            NthExpr::AnOpB(1, Some(NthExprOp::Add), 1)
        );
    }

    #[test]
    fn test_convert_to_nth_expr4() {
        assert_eq!(
            // 2n + 1
            NthExpr::from(SPNthExpr::AnOpB(
                (0, 1, 1),
                Some(Token::Number((0, 1, 1), 2)),
                Some(SPNthExprOp::Add((0, 1, 1))),
                Some(Token::Number((0, 1, 1), 1))
            )),
            NthExpr::AnOpB(2, Some(NthExprOp::Add), 1)
        );
    }

    #[test]
    fn test_convert_to_nth_expr_even() {
        assert_eq!(
            // even
            NthExpr::from(SPNthExpr::Even((0, 1, 1))),
            NthExpr::AnOpB(2, None, 0)
        );
    }

    #[test]
    fn test_convert_to_nth_expr_odd() {
        assert_eq!(
            // odd
            NthExpr::from(SPNthExpr::Odd((0, 1, 1))),
            NthExpr::AnOpB(2, Some(NthExprOp::Add), 1)
        );
    }

    #[test]
    fn test_convert_to_selector() {
        assert_eq!(
            Selector::from(SPSelector::Seq(vec![
                SPSelector::Simple(SPSimpleSelector::new(
                    (0, 1, 1),
                    Some(ElemType::A),
                    Some(Token::AttrIdentifier((0, 1, 1), "An-Id".to_string())),
                    vec![
                        Token::Str((0, 1, 1), "cl1".to_string()),
                        Token::Str((0, 1, 1), "CL2".to_string()),
                    ],
                    false,
                )),
                SPSelector::Attr(SPAttrSelector::new(
                    (0, 1, 1),
                    Token::AttrIdentifier((0, 1, 1), "HrEf".to_string()),
                    Some((
                        SPAttrSelectorOp::ContainsAtLeastOne((0, 1, 1)),
                        Token::Str((0, 1, 1), "href-str".to_string()),
                    )),
                    true,
                )),
            ])),
            Selector::Seq(vec![
                Selector::Simple(SimpleSelector::new(
                    Some(ElemType::A),
                    Some("An-Id".to_string()),
                    hashset! { "cl1".to_string(), "CL2".to_string() },
                    false,
                )),
                Selector::Attr(AttrSelector::new(
                    "href".to_string(),
                    Some((AttrSelectorOp::ContainsAtLeastOne, "href-str".to_string())),
                    true,
                )),
            ])
        );
    }

    #[test]
    fn test_convert_to_css_blocks() {
        assert_eq!(
            CssBlocks::from(CPCssBlocks(vec![
                (
                    SPSelector::Simple(SPSimpleSelector::new(
                        (0, 1, 1),
                        Some(ElemType::A),
                        None,
                        vec![],
                        false,
                    )),
                    vec![
                        (
                            CPToken::Property((0, 1, 1), "AttR1".to_string()),
                            CPToken::Value((0, 1, 1), "val1".to_string()),
                        ),
                        (
                            CPToken::Property((0, 1, 1), "AttR2".to_string()),
                            CPToken::Value((0, 1, 1), "val2".to_string()),
                        ),
                    ],
                ),
                (
                    SPSelector::Simple(SPSimpleSelector::new(
                        (0, 1, 1),
                        Some(ElemType::A),
                        None,
                        vec![],
                        false,
                    )),
                    vec![(
                        CPToken::Property((0, 1, 1), "AtTr2".to_string()),
                        CPToken::Value((0, 1, 1), "val3".to_string()),
                    )],
                ),
            ])),
            CssBlocks(vec![(
                Selector::Simple(SimpleSelector::new(
                    Some(ElemType::A),
                    None,
                    hashset!{},
                    false,
                )),
                hashmap! {
                    "attr1".to_string() => "val1".to_string(),
                    "attr2".to_string() => "val3".to_string()
                },
            )])
        );
    }

    #[test]
    fn test_child_index1() {
        let parent =
            DomNode::new(ElemType::A, None, hashset!{}, hashmap!{}, None, vec![]).to_dnref();
        parent.add_children(vec![
            DomNode::new(
                ElemType::A,
                Some("0".to_string()),
                hashset!{},
                hashmap!{},
                None,
                vec![],
            ).to_dnref(),
            DomNode::new(
                ElemType::A,
                Some("1".to_string()),
                hashset!{},
                hashmap!{},
                None,
                vec![],
            ).to_dnref(),
        ]);
        assert_eq!(parent.borrow().children[1].child_index(), Some(2));
    }

    #[test]
    fn test_child_index2() {
        let parent =
            DomNode::new(ElemType::A, None, hashset!{}, hashmap!{}, None, vec![]).to_dnref();
        parent.add_children(vec![
            DomNode::new(
                ElemType::A,
                Some("0".to_string()),
                hashset!{"cl1".to_string()},
                hashmap!{"class".to_string() => Some("cl1".to_string())},
                None,
                vec![],
            ).to_dnref(),
            DomNode::new(
                ElemType::A,
                Some("1".to_string()),
                hashset!{},
                hashmap!{},
                None,
                vec![],
            ).to_dnref(),
        ]);
        assert_eq!(parent.borrow().children[0].child_index(), Some(1));
    }

    #[test]
    fn test_child_index3() {
        let parent =
            DomNode::new(ElemType::A, None, hashset!{}, hashmap!{}, None, vec![]).to_dnref();
        parent.add_children(vec![
            DomNode::new(
                ElemType::A,
                Some("0".to_string()),
                hashset!{"cl1".to_string()},
                hashmap!{"class".to_string() => Some("cl1".to_string())},
                None,
                vec![],
            ).to_dnref(),
            DomNode::new(
                ElemType::A,
                Some("1".to_string()),
                hashset!{},
                hashmap!{},
                None,
                vec![],
            ).to_dnref(),
        ]);
        assert_eq!(parent.child_index(), None);
    }

    #[test]
    fn test_nthexpr_matches_a() {
        let expr = NthExpr::A(3);
        assert!(!expr.matches(2));
        assert!(expr.matches(3));
    }

    #[test]
    fn test_nthexpr_matches_positive_an() {
        let expr = NthExpr::AnOpB(1, None, 0);
        assert!(expr.matches(1));
        assert!(expr.matches(2));
        assert!(expr.matches(3));
        assert!(expr.matches(4));

        let expr = NthExpr::AnOpB(2, None, 0);
        assert!(!expr.matches(1));
        assert!(expr.matches(2));
        assert!(!expr.matches(3));
        assert!(expr.matches(4));

        let expr = NthExpr::AnOpB(3, None, 0);
        assert!(!expr.matches(1));
        assert!(!expr.matches(2));
        assert!(expr.matches(3));
        assert!(!expr.matches(4));
    }

    #[test]
    fn test_nthexpr_matches_negative_an() {
        let expr = NthExpr::AnOpB(-1, None, 0);
        assert!(!expr.matches(1));
        assert!(!expr.matches(2));
        assert!(!expr.matches(3));
        assert!(!expr.matches(4));

        let expr = NthExpr::AnOpB(-2, None, 0);
        assert!(!expr.matches(1));
        assert!(!expr.matches(2));
        assert!(!expr.matches(3));
        assert!(!expr.matches(4));

        let expr = NthExpr::AnOpB(-3, None, 0);
        assert!(!expr.matches(1));
        assert!(!expr.matches(2));
        assert!(!expr.matches(3));
        assert!(!expr.matches(4));
    }

    #[test]
    fn test_nthexpr_matches_an_plus_b() {
        let expr = NthExpr::AnOpB(1, Some(NthExprOp::Add), 2);
        assert!(!expr.matches(1));
        assert!(expr.matches(2));
        assert!(expr.matches(3));
        assert!(expr.matches(4));

        let expr = NthExpr::AnOpB(-1, Some(NthExprOp::Add), 2);
        assert!(expr.matches(1));
        assert!(expr.matches(2));
        assert!(!expr.matches(3));
        assert!(!expr.matches(4));

        let expr = NthExpr::AnOpB(2, Some(NthExprOp::Add), 1);
        assert!(expr.matches(1));
        assert!(!expr.matches(2));
        assert!(expr.matches(3));
        assert!(!expr.matches(4));

        let expr = NthExpr::AnOpB(-2, Some(NthExprOp::Add), 3);
        assert!(expr.matches(1));
        assert!(!expr.matches(2));
        assert!(expr.matches(3));
        assert!(!expr.matches(4));
        assert!(!expr.matches(5));
    }

    #[test]
    fn test_nthexpr_matches_an_sub_b() {
        let expr = NthExpr::AnOpB(1, Some(NthExprOp::Sub), 3);
        assert!(expr.matches(1));
        assert!(expr.matches(2));
        assert!(expr.matches(3));
        assert!(expr.matches(4));

        let expr = NthExpr::AnOpB(2, Some(NthExprOp::Sub), 1);
        assert!(expr.matches(1));
        assert!(!expr.matches(2));
        assert!(expr.matches(3));
        assert!(!expr.matches(4));

        let expr = NthExpr::AnOpB(-2, Some(NthExprOp::Sub), 1);
        assert!(!expr.matches(1));
        assert!(!expr.matches(2));
        assert!(!expr.matches(3));
        assert!(!expr.matches(4));
        assert!(!expr.matches(5));
    }
}
