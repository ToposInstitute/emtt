use std::rc::Rc;
use std::{cmp::Ordering, hash::Hash};

use super::*;
use crate::env::Env;

#[derive(Clone, Debug)]
pub enum Type {
    Equals(Elt, Elt),
    Record(Env, Tele),
    Neu(Rc<model::Neutral>),
}

// We need this because we put Type in the App node for e-graphs,
// but we don't actually care about it
//
// If we really wanted to check equality of types, we'd have to
// do something like intro variables and eval the telescope, but
// I don't think we'll ever have to do this? Famous last words...
impl PartialEq for Type {
    fn eq(&self, _other: &Type) -> bool {
        true
    }
}

impl Eq for Type {}

impl PartialOrd for Type {
    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        Some(Ordering::Equal)
    }
}

impl Ord for Type {
    fn cmp(&self, _other: &Self) -> Ordering {
        Ordering::Equal
    }
}

impl Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {}
}
