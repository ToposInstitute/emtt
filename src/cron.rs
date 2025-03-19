use egg::{Analysis, EGraph, Id};
use std::collections::HashMap;
use std::rc::Rc;

use crate::core::*;

#[derive(Clone, Debug)]
pub struct TypeData {
    pub tp: Rc<Type>,
    pub fieldtps: Option<Vec<Rc<Type>>>,
}

impl TypeData {
    pub fn new(tp: Rc<Type>, fieldtps: Option<Vec<Rc<Type>>>) -> Self {
        Self { tp, fieldtps }
    }
}

#[derive(Clone, Debug)]
pub struct TypeAnalysis {
    pub envtps: HashMap<Lvl, TypeData>,
}

impl TypeAnalysis {
    pub fn new() -> Self {
        Self {
            envtps: HashMap::new(),
        }
    }
}

impl Analysis<element::Neutral> for TypeAnalysis {
    type Data = TypeData;

    fn make(egraph: &mut EGraph<element::Neutral, Self>, enode: &element::Neutral) -> Self::Data {
        match enode {
            element::Neutral::Var(l) => egraph.analysis.envtps.get(l).unwrap().clone(),
            element::Neutral::Proj([x], field) => TypeData {
                tp: egraph[*x].data.fieldtps.as_ref().unwrap()[field.lvl()].clone(),
                fieldtps: None,
            },
            element::Neutral::ModelProj(_, _, tp, _) => TypeData {
                tp: tp.clone(),
                fieldtps: None,
            },
            element::Neutral::App(_, _, tp, _) => TypeData {
                tp: tp.clone(),
                fieldtps: None,
            },
        }
    }

    fn merge(&mut self, a: &mut Self::Data, b: Self::Data) -> egg::DidMerge {
        match (&a.fieldtps, b.fieldtps) {
            (None, Some(fields)) => {
                a.fieldtps = Some(fields);
                egg::DidMerge(true, false)
            }
            _ => egg::DidMerge(false, false),
        }
    }
}

/// Congruence Relation on Neutrals
pub type CRoN = EGraph<element::Neutral, TypeAnalysis>;
