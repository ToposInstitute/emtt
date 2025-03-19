use std::{collections::HashMap, rc::Rc};

use bumpalo::Bump;
use egg::EGraph;
use fexplib::{grammar::parse, lexer::lex, parser::Prec, types::FExp};
use tattle::Reporter;

use crate::{
    core::*,
    cron::TypeAnalysis,
    ctx::{Binding, Ctx},
    elab::Elaborator,
    env::Env,
    eval::Evaluator,
    symtable::SymTable,
};

pub struct ModelSq {
    pub args: Tele,
    pub theory: TheoryStx,
    pub body: ModelStx,
}

impl ModelSq {
    pub fn new(args: Tele, theory: TheoryStx, body: ModelStx) -> Self {
        Self { args, theory, body }
    }
}

pub struct TheorySq {
    pub args: Tele,
    pub body: Tele,
}

impl TheorySq {
    pub fn new(args: Tele, body: Tele) -> Self {
        Self { args, body }
    }
}

pub enum Sequent {
    Theory(TheorySq),
    Model(ModelSq),
}

pub struct Toplevel {
    sequents: Vec<(Name, Sequent)>,
}

impl Toplevel {
    pub fn empty() -> Self {
        Toplevel {
            sequents: Vec::new(),
        }
    }

    pub fn sequent(&self, name: TopName) -> &Sequent {
        &self.sequents[name.lvl()].1
    }

    pub fn try_model(&self, name: TopName) -> Option<&ModelSq> {
        match self.sequent(name) {
            Sequent::Model(m) => Some(m),
            _ => None,
        }
    }

    pub fn model(&self, name: TopName) -> &ModelSq {
        self.try_model(name).expect("expected theory")
    }

    pub fn try_theory(&self, name: TopName) -> Option<&TheorySq> {
        match self.sequent(name) {
            Sequent::Theory(t) => Some(t),
            _ => panic!("expected theory"),
        }
    }

    pub fn theory(&self, name: TopName) -> &TheorySq {
        self.try_theory(name).expect("expected theory")
    }
}

pub struct TopElaborator {
    reporter: Reporter,
    toplevel: Toplevel,
    symtable: SymTable<Binding>,
}

impl TopElaborator {
    pub fn empty(reporter: Reporter) -> Self {
        TopElaborator {
            reporter,
            toplevel: Toplevel::empty(),
            symtable: SymTable::empty(),
        }
    }

    pub fn elab_sequent(&mut self, e: &FExp) -> Option<()> {
        let (name, sequent) = {
            let evaluator = Evaluator::new(&self.toplevel, EGraph::new(TypeAnalysis::new()));
            let mut ctx = Ctx::new(Env::empty(), Rc::new(evaluator), &mut self.symtable);
            let elab = Elaborator::new(self.reporter.clone());
            elab.sequent(&mut ctx, e)
        }?;
        let tn = TopName::new(Name(Some(name)), self.toplevel.sequents.len());
        self.symtable.insert(name, Binding::Toplevel(tn));
        self.toplevel.sequents.push((Name(Some(name)), sequent));
        Some(())
    }
}

const PRECTABLE: &[(&str, Prec)] = &[
    ("=", Prec::nonassoc(10)),
    (":", Prec::nonassoc(20)),
    ("==", Prec::nonassoc(30)),
    ("->", Prec::nonassoc(30)),
    ("↦", Prec::nonassoc(30)),
    ("+", Prec::lassoc(50)),
    ("*", Prec::lassoc(60)),
];
const KEYWORDTABLE: &[&str] = &["=", ":", "==", "↦", "->"];

pub fn with_parsed<T, F: FnMut(&FExp) -> T>(
    input: &str,
    reporter: Reporter,
    mut f: F,
) -> Option<T> {
    let prectable: HashMap<_, _> = PRECTABLE
        .iter()
        .map(|(name, p)| (name.to_string(), *p))
        .collect();
    let tokens = lex(input, KEYWORDTABLE, reporter.clone());
    let arena = Bump::new();
    let ast = parse(input, reporter.clone(), &prectable, &tokens, &arena);
    if reporter.errored() {
        None
    } else {
        Some(f(ast))
    }
}
