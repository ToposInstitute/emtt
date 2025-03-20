use egg::Id;

use super::*;
use crate::{env::Env, eval::Evaluator};
use std::rc::Rc;

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Neutral {
    Var(Lvl),
    Proj(Rc<Neutral>, Field),
    App(Rc<Neutral>, Vec<Elt>),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum FlattenedNeutral {
    Var(Lvl),
    Proj(Rc<FlattenedNeutral>, Field),
    App(Rc<FlattenedNeutral>, Vec<FlattenedElt>),
}

impl Neutral {
    pub fn flatten_into(&self, out: &mut Vec<Id>) -> FlattenedNeutral {
        use Neutral::*;
        match self {
            Var(l) => FlattenedNeutral::Var(*l),
            Proj(n, f) => FlattenedNeutral::Proj(Rc::new(n.flatten_into(out)), *f),
            App(head, args) => {
                let head = head.flatten_into(out);
                let args = args.iter().map(|arg| arg.flatten_into(out)).collect();
                FlattenedNeutral::App(Rc::new(head), args)
            }
        }
    }

    // pub fn unflatten(&self, from: &[Id]) -> Self {
    //     use Neutral::*;
    //     match self {
    //         Var(l) => Var(*l),
    //         Proj(n, f) => Proj(Rc::new(n.unflatten(from)), *f),
    //         App(head, args) => {
    //             let head = head.unflatten(from);
    //             let args = args.iter().map(|arg| arg.unflatten(from)).collect();
    //             App(Rc::new(head), args)
    //         }
    //     }
    // }
}

#[derive(Clone, Debug)]
pub enum Model {
    Elt(Elt),

    Neu(Rc<Neutral>, Theory),
    Cons(Rc<Vec<Model>>),
    Lam(Env, Rc<Lam>),

    Type(Type),
}

impl From<Elt> for Model {
    fn from(e: Elt) -> Self {
        Model::Elt(e)
    }
}

impl<'a> From<&'a Elt> for Model {
    fn from(e: &'a Elt) -> Self {
        Model::Elt(e.clone())
    }
}

impl Model {
    pub fn as_elt(self) -> Elt {
        match self {
            Model::Elt(e) => e,
            _ => panic!("expected an element"),
        }
    }

    pub fn as_type(self) -> Type {
        match self {
            Model::Type(t) => t,
            Model::Neu(n, Theory::Tp) => Type::Neu(n.clone()),
            _ => panic!("expected a type"),
        }
    }

    pub fn model_app(&self, evaluator: &mut Evaluator, args: Vec<Elt>) -> Model {
        match self {
            Model::Neu(n, th) => {
                let (env, pi) = th.as_pi();
                let ret_th = evaluator.eval_theory(&env.extend(args.iter()), &pi.ret);
                Model::Neu(Rc::new(Neutral::App(n.clone(), args)), ret_th)
            }
            Model::Lam(e, l) => evaluator.eval_model(&e.extend(args.iter()), &l.body),
            _ => panic!("can only apply neutral or lambda"),
        }
    }

    pub fn elt_app(&self, evaluator: &mut Evaluator, args: Vec<Elt>) -> Elt {
        match self {
            Model::Neu(n, th) => {
                let mut flattened = Vec::new();
                let n_for_egraph = n.flatten_into(&mut flattened);
                let args_for_egraph: Vec<_> = args
                    .iter()
                    .map(|arg| arg.flatten_into(&mut flattened))
                    .collect();
                let (env, pi) = th.as_pi();
                let tp = Rc::new(evaluator.eval_type(&env.extend(args.iter()), &pi.ret));
                let i = evaluator.cron.add(element::Neutral::App(
                    Rc::new(n_for_egraph),
                    Rc::new(args_for_egraph),
                    tp.clone(),
                    flattened,
                ));
                evaluator.eta_expand_elt(i, &*tp)
            }
            Model::Lam(e, l) => evaluator.eval_elt(&e.extend(args.iter()), &l.body),
            _ => panic!("can only apply neutral or lambda"),
        }
    }

    pub fn proj(&self, field: &Field) -> Model {
        match self {
            Model::Cons(fields) => fields[field.lvl()].clone(),
            Model::Elt(Elt::Cons(fields)) => Model::Elt(fields[field.lvl()].clone()),
            Model::Neu(_, _) => panic!("expected neutrals of record type to be eta-expanded"),
            _ => panic!("can only project from neutral or cons"),
        }
    }
}
