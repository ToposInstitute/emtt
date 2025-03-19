use super::*;
use egg::{Id, Language};
use std::rc::Rc;

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Elt {
    // used when element show up in the head of an egraph node, refers to one of
    // the arguments of the e-graph node. Otherwise should not appear.
    Lvl(usize),
    Id(Id),
    Erased,
    Cons(Rc<Vec<Elt>>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Neutral {
    Var(Lvl),
    Proj([Id; 1], Field),
    ModelProj(Rc<model::Neutral>, Field, Rc<Type>, Vec<Id>),
    App(Rc<model::Neutral>, Rc<Vec<Elt>>, Rc<Type>, Vec<Id>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Discriminant {
    Var(Lvl),
    Proj(Field),
    App(Rc<model::Neutral>),
    ModelProj(Rc<model::Neutral>, Field),
}

impl Elt {
    pub fn flatten_into(&self, out: &mut Vec<Id>) -> Elt {
        match self {
            Elt::Lvl(_) => panic!("expected a regular element"),
            Elt::Id(i) => {
                let j = out.len();
                out.push(*i);
                Elt::Lvl(j)
            }
            Elt::Erased => Elt::Erased,
            Elt::Cons(fields) => Elt::Cons(Rc::new(
                fields.iter().map(|field| field.flatten_into(out)).collect(),
            )),
        }
    }

    pub fn unflatten(&self, from: &[Id]) -> Elt {
        match self {
            Elt::Lvl(i) => Elt::Id(from[*i]),
            Elt::Id(_) => panic!("expected a flattened element"),
            Elt::Erased => Elt::Erased,
            Elt::Cons(fields) => Elt::Cons(Rc::new(
                fields.iter().map(|field| field.unflatten(from)).collect(),
            )),
        }
    }
}

impl Language for Neutral {
    type Discriminant = Discriminant;

    fn discriminant(&self) -> Self::Discriminant {
        use Neutral::*;
        match self {
            Var(l) => Discriminant::Var(*l),
            Proj(_, f) => Discriminant::Proj(*f),
            ModelProj(n, f, _, _) => Discriminant::ModelProj(n.clone(), *f),
            App(f, _, _, _) => Discriminant::App(f.clone()),
        }
    }

    fn matches(&self, other: &Self) -> bool {
        self.discriminant() == other.discriminant()
    }

    fn children(&self) -> &[Id] {
        use Neutral::*;
        match self {
            Var(_) => &[],
            Proj(x, _) => x,
            ModelProj(_, _, _, flattened) => flattened.as_slice(),
            App(_, _, _, flattened) => flattened.as_slice(),
        }
    }

    fn children_mut(&mut self) -> &mut [Id] {
        use Neutral::*;
        match self {
            Var(_) => &mut [],
            Proj(x, _) => x,
            ModelProj(_, _, _, flattened) => flattened.as_mut_slice(),
            App(_, _, _, flattened) => flattened.as_mut_slice(),
        }
    }
}
