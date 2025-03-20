use ustr::Ustr;

use crate::{
    core::*,
    cron::TypeData,
    env::Env,
    eval::Evaluator,
    symtable::{self, SymTable},
    toplevel::Toplevel,
};
use std::rc::Rc;

#[derive(Clone)]
pub enum Binding {
    Toplevel(TopName),
    Local(Lvl, Theory),
}

pub struct Checkpoint<'a> {
    env: Env,
    evaluator: Rc<Evaluator<'a>>,
    symtable: symtable::Checkpoint,
}

pub struct CheckpointLite {
    env: Env,
    symtable: symtable::Checkpoint,
}

pub struct Ctx<'a> {
    pub env: Env,
    pub evaluator: Rc<Evaluator<'a>>,
    pub symtable: &'a mut SymTable<Binding>,
}

impl<'a> Ctx<'a> {
    pub fn new(
        env: Env,
        evaluator: Rc<Evaluator<'a>>,
        symtable: &'a mut SymTable<Binding>,
    ) -> Self {
        Self {
            env,
            evaluator,
            symtable,
        }
    }

    pub fn checkpoint(&self) -> Checkpoint<'a> {
        Checkpoint {
            env: self.env.clone(),
            evaluator: self.evaluator.clone(),
            symtable: self.symtable.checkpoint(),
        }
    }

    pub fn checkpoint_lite(&self) -> CheckpointLite {
        CheckpointLite {
            env: self.env.clone(),
            symtable: self.symtable.checkpoint(),
        }
    }

    pub fn return_to(&mut self, c: Checkpoint<'a>) {
        self.env = c.env;
        self.evaluator = c.evaluator;
        self.symtable.return_to(c.symtable);
    }

    pub fn return_to_lite(&mut self, c: CheckpointLite) {
        self.env = c.env;
        self.symtable.return_to(c.symtable);
    }

    pub fn evaluator_mut(&mut self) -> &mut Evaluator<'a> {
        Rc::<Evaluator<'a>>::make_mut(&mut self.evaluator)
    }

    pub fn add_local_binding(&mut self, name: Name, lvl: Lvl, th: &Theory) {
        match name.0 {
            Some(n) => self.symtable.insert(n, Binding::Local(lvl, th.clone())),
            None => {}
        }
    }

    pub fn intro_model(&mut self, name: Name, theory: &Theory) {
        let l = Lvl::new(name, self.env.len());
        let m = self
            .evaluator_mut()
            .eta_expand_model(Rc::new(model::Neutral::Var(l)), theory);
        self.add_local_binding(name, l, theory);
        self.env = self.env.push(m.clone());
    }

    pub fn toplevel(&self) -> &'a Toplevel {
        &self.evaluator.as_ref().toplevel
    }

    pub fn topname(&self, n: Ustr) -> Option<TopName> {
        match self.lookup(n) {
            Some(Binding::Toplevel(tn)) => Some(tn),
            _ => None,
        }
    }

    pub fn intro_elt(&mut self, name: Name, tp: &Type) -> Elt {
        let l = Lvl::new(name, self.env.len());
        self.evaluator_mut()
            .cron
            .analysis
            .envtps
            .insert(l, TypeData::new(Rc::new(tp.clone()), None));
        let i = self.evaluator_mut().cron.add(element::Neutral::Var(l));
        let e = self.evaluator_mut().eta_expand_elt(i, tp);
        self.add_local_binding(name, l, &Theory::Type(tp.clone()));
        self.env = self.env.push(Model::Elt(e.clone()));
        e
    }

    pub fn lookup(&self, name: Ustr) -> Option<Binding> {
        self.symtable.get(&name).cloned()
    }

    pub fn lvl_to_idx(&self, lvl: Lvl) -> Idx {
        Idx::new(lvl.name(), self.env.len() - lvl.lvl() - 1)
    }

    pub fn convertable_theories(&mut self, th1: &Theory, th2: &Theory) -> bool {
        match (th1, th2) {
            (Theory::Tp, Theory::Tp) => true,
            (Theory::Pi(_env1, _pi1), Theory::Pi(_env2, _pi2)) => {
                todo!("conversion checking for pi types")
            }
            (Theory::TopApp(tn1, args1), Theory::TopApp(tn2, args2)) => {
                tn1 == tn2
                    && args1
                        .iter()
                        .zip(args2.iter())
                        .all(|(m1, m2)| self.convertable_models(m1, m2))
            }
            (Theory::Type(t1), Theory::Type(t2)) => self.convertable_types(t1, t2),
            _ => false,
        }
    }

    pub fn convertable_types(&mut self, tp1: &Type, tp2: &Type) -> bool {
        match (tp1, tp2) {
            (Type::Equals(l1, r1), Type::Equals(l2, r2)) => {
                self.convertable_elts(l1, l2) && self.convertable_elts(r1, r2)
            }
            // TODO: this is quite expensive, is there a way to shortcircuit?
            (Type::Record(env1, tele1), Type::Record(env2, tele2)) => {
                let c = self.checkpoint();
                let mut env1 = env1.clone();
                let mut env2 = env2.clone();
                for ((name, tpstx1), (_, tpstx2)) in tele1.iter().zip(tele2.iter()) {
                    let tp1 = self.evaluator_mut().eval_type(&env1, tpstx1);
                    let tp2 = self.evaluator_mut().eval_type(&env1, tpstx2);
                    if !self.convertable_types(&tp1, &tp2) {
                        self.return_to(c);
                        return false;
                    }
                    let e = self.intro_elt(*name, &tp1);
                    env1 = env1.push(Model::Elt(e.clone()));
                    env2 = env2.push(Model::Elt(e.clone()));
                }
                true
            }
            (Type::Neu(m1), Type::Neu(m2)) => self.convertable_model_neutrals(m1, m2),
            _ => false,
        }
    }

    pub fn convertable_model_neutrals(&mut self, m1: &model::Neutral, m2: &model::Neutral) -> bool {
        match (m1, m2) {
            (model::Neutral::App(n1, args1), model::Neutral::App(n2, args2)) => {
                self.convertable_model_neutrals(n1, n2)
                    && args1
                        .iter()
                        .zip(args2.iter())
                        .all(|(arg1, arg2)| self.convertable_elts(arg1, arg2))
            }
            (model::Neutral::Proj(n1, f1), model::Neutral::Proj(n2, f2)) => {
                self.convertable_model_neutrals(n1, n2) && f1 == f2
            }
            (model::Neutral::Var(l1), model::Neutral::Var(l2)) => l1 == l2,
            _ => false,
        }
    }

    pub fn convertable_models(&mut self, m1: &Model, m2: &Model) -> bool {
        match (m1, m2) {
            (Model::Type(t1), Model::Type(t2)) => self.convertable_types(t1, t2),
            (Model::Elt(e1), Model::Elt(e2)) => self.convertable_elts(e1, e2),
            (Model::Neu(n1, _), Model::Neu(n2, _)) => self.convertable_model_neutrals(n1, n2),
            (Model::Lam(_e1, _l1), Model::Lam(_e2, _l2)) => {
                todo!("conversion checking for lambdas")
            }
            _ => false,
        }
    }

    pub fn convertable_elts(&self, e1: &Elt, e2: &Elt) -> bool {
        match (e1, e2) {
            (Elt::Erased, Elt::Erased) => true,
            (Elt::Id(i1), Elt::Id(i2)) => {
                self.evaluator.cron.find(*i1) == self.evaluator.cron.find(*i2)
            }
            (Elt::Cons(fields1), Elt::Cons(fields2)) => fields1
                .iter()
                .zip(fields2.iter())
                .all(|(e1, e2)| self.convertable_elts(e1, e2)),
            _ => panic!("tried to conversion-check terms of different types"),
        }
    }

    pub fn let_bind(&mut self, name: Name, m: Model, th: &Theory) {
        let l = Lvl::new(name, self.env.len());
        self.env = self.env.push(m);
        self.add_local_binding(name, l, th);
    }
}
