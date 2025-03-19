use egg::Id;
use model::Neutral;

use crate::{
    core::*,
    cron::{CRoN, TypeData},
    env::Env,
    toplevel::{Sequent, Toplevel},
};
use std::rc::Rc;

#[derive(Clone)]
pub struct Evaluator<'a> {
    pub toplevel: &'a Toplevel,
    pub cron: CRoN,
}

impl<'a> Evaluator<'a> {
    pub fn new(toplevel: &'a Toplevel, cron: CRoN) -> Self {
        Self { toplevel, cron }
    }

    pub fn eval_elt(&mut self, env: &Env, stx: &EltStx) -> Elt {
        self.eval_model(env, stx).as_elt()
    }

    pub fn eval_model(&mut self, env: &Env, stx: &ModelStx) -> Model {
        match stx {
            Stx::Var(i) => env.get(*i),
            Stx::Refl => Model::Elt(Elt::Erased),
            Stx::Equals(e1, e2) => {
                Model::Type(Type::Equals(self.eval_elt(env, e1), self.eval_elt(env, e2)))
            }
            Stx::EltApp(f, args) => {
                let f = self.eval_model(env, f);
                let args: Vec<Elt> = args.iter().map(|arg| self.eval_elt(env, arg)).collect();
                Model::Elt(f.elt_app(self, args))
            }
            Stx::ModelApp(f, args) => {
                let f = self.eval_model(env, f);
                let args: Vec<Elt> = args.iter().map(|arg| self.eval_elt(env, arg)).collect();
                f.model_app(self, args)
            }
            Stx::TopApp(tn, args) => {
                let m = self.toplevel.model(*tn);
                let args: Vec<_> = args.iter().map(|arg| self.eval_model(env, arg)).collect();
                self.eval_model(&args.into(), &m.body)
            }
            Stx::Tp => panic!("type is a theory"),
            Stx::Pi(_) => panic!("pi is a theory"),
            Stx::Lam(l) => Model::Lam(env.clone(), l.clone()),
            Stx::Record(r) => Model::Type(Type::Record(env.clone(), r.clone())),
            Stx::ModelCons(tele) => {
                let mut env = env.clone();
                let mut fields = Vec::new();
                for (_, stx) in tele.iter() {
                    let m = self.eval_model(&env, stx);
                    env = env.push(m.clone());
                    fields.push(m)
                }
                Model::Cons(Rc::new(fields))
            }
            Stx::EltCons(tele) => {
                let mut env = env.clone();
                let mut fields = Vec::new();
                for (_, stx) in tele.iter() {
                    let e = self.eval_elt(&env, stx);
                    env = env.push(Model::Elt(e.clone()));
                    fields.push(e)
                }
                Model::Elt(Elt::Cons(Rc::new(fields)))
            }
            Stx::Proj(x, field) => self.eval_model(env, x).proj(field),
            Stx::Block(bindings, ret) => {
                let mut env = env.clone();
                for (_, stx) in bindings.iter() {
                    let m = self.eval_model(&env, stx);
                    env = env.push(m);
                }
                self.eval_model(&env, ret)
            }
        }
    }

    pub fn eval_theory(&mut self, env: &Env, stx: &TheoryStx) -> Theory {
        match stx {
            Stx::TopApp(tn, args) => match self.toplevel.sequent(*tn) {
                Sequent::Theory(_) => {
                    let args = args.iter().map(|arg| self.eval_model(env, arg)).collect();
                    Theory::TopApp(*tn, Rc::new(args))
                }
                Sequent::Model(_) => Theory::Type(self.eval_model(env, stx).as_type()),
            },
            Stx::Tp => Theory::Tp,
            Stx::Pi(pi) => Theory::Pi(env.clone(), pi.clone()),
            _ => Theory::Type(self.eval_model(env, stx).as_type()),
        }
    }

    pub fn eval_type(&mut self, env: &Env, stx: &TheoryStx) -> Type {
        self.eval_model(env, stx).as_type()
    }

    pub fn union(&mut self, e1: &Elt, e2: &Elt) {
        match (e1, e2) {
            (Elt::Id(i1), Elt::Id(i2)) => {
                self.cron.union(*i1, *i2);
            }
            (Elt::Cons(fields1), Elt::Cons(fields2)) => fields1
                .iter()
                .zip(fields2.iter())
                .for_each(|(e1, e2)| self.union(e1, e2)),
            (Elt::Erased, Elt::Erased) => {}
            _ => panic!("tried to union elements of different types"),
        }
    }

    pub fn eta_expand_elt(&mut self, i: Id, tp: &Type) -> Elt {
        match tp {
            Type::Equals(e1, e2) => {
                self.union(e1, e2);
                Elt::Erased
            }
            Type::Record(env, tele) => {
                let mut env = env.clone();
                let mut fields = Vec::new();
                self.cron[i].data.fieldtps = Some(Vec::new());
                for (lvl, (name, tpstx)) in tele.iter().enumerate() {
                    let tp = Rc::new(self.eval_type(&env, tpstx));
                    self.cron[i]
                        .data
                        .fieldtps
                        .as_mut()
                        .unwrap()
                        .push(tp.clone());
                    let j = self
                        .cron
                        .add(element::Neutral::Proj([i], Field::new(*name, lvl)));
                    let pe = self.eta_expand_elt(j, &tp);
                    env = env.push(Model::Elt(pe.clone()));
                    fields.push(pe);
                }
                Elt::Cons(Rc::new(fields))
            }
            Type::Neu(_) => Elt::Id(i),
        }
    }

    pub fn eta_expand_model(&mut self, m: Rc<model::Neutral>, th: &Theory) -> Model {
        match th {
            Theory::Tp => Model::Neu(m, th.clone()),
            // we don't need to eta-expand Pi types, as far as I can tell
            Theory::Pi(_, _) => Model::Neu(m, th.clone()),
            Theory::TopApp(tn, args) => {
                let theory = self.toplevel.theory(*tn);
                let mut env: Env = args.iter().cloned().into();
                let mut fields = Vec::new();
                for (i, (name, thstx)) in theory.body.iter().enumerate() {
                    let th = self.eval_theory(&env, thstx);
                    let m = self.eta_expand_model(
                        Rc::new(Neutral::Proj(m.clone(), Field::new(*name, i))),
                        &th,
                    );
                    env = env.push(m.clone());
                    fields.push(m);
                }
                Model::Cons(Rc::new(fields))
            }
            Theory::Type(t) => match &*m {
                model::Neutral::Var(l) => {
                    self.cron
                        .analysis
                        .envtps
                        .insert(*l, TypeData::new(Rc::new(t.clone()), None));
                    let i = self.cron.add(element::Neutral::Var(*l));
                    Model::Elt(self.eta_expand_elt(i, t))
                }
                model::Neutral::Proj(n, f) => {
                    let mut flattened = Vec::new();
                    let n = n.flatten_into(&mut flattened);
                    let i = self.cron.add(element::Neutral::ModelProj(
                        Rc::new(n),
                        *f,
                        Rc::new(t.clone()),
                        flattened,
                    ));
                    Model::Elt(self.eta_expand_elt(i, &t))
                }
                model::Neutral::App(n, args) => {
                    Model::Elt(Model::Neu(n.clone(), th.clone()).elt_app(self, args.clone()))
                }
            },
        }
    }
}
