use super::*;
use crate::env::Env;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum Theory {
    Tp,
    Pi(Env, Rc<Pi>),
    TopApp(TopName, Rc<Vec<Model>>),
    Type(Type),
}

impl Theory {
    pub fn as_pi(&self) -> (Env, Rc<Pi>) {
        match self {
            Theory::Pi(e, pi) => (e.clone(), pi.clone()),
            _ => panic!("expected pi type"),
        }
    }

    pub fn is_type(&self) -> bool {
        match self {
            Theory::Type(_) => true,
            _ => false,
        }
    }
}
