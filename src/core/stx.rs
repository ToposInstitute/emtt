use super::names::*;
use std::rc::Rc;
use ustr::Ustr;

#[derive(Clone, Debug)]
pub struct Tele(Rc<Vec<(Name, Stx)>>);

impl Tele {
    pub fn from_vec(v: Vec<(Name, Stx)>) -> Self {
        Tele(Rc::new(v))
    }

    pub fn iter(&self) -> impl Iterator<Item = &(Name, Stx)> {
        self.0.iter()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn find_field(&self, name: Ustr) -> Option<Field> {
        let name = Name(Some(name));
        self.0
            .iter()
            .enumerate()
            .find(|(_, (n, _))| *n == name)
            .map(|(i, _)| Field::new(name, i))
    }

    pub fn get(&self, field: Field) -> &(Name, Stx) {
        &self.0[field.lvl()]
    }
}

#[derive(Debug)]
pub enum Stx {
    Var(Idx),

    Refl,
    Equals(Rc<EltStx>, Rc<EltStx>),

    EltApp(Rc<ModelStx>, Vec<EltStx>),
    ModelApp(Rc<ModelStx>, Vec<EltStx>),
    TopApp(TopName, Vec<ModelStx>),

    Tp,

    Pi(Rc<Pi>),
    Lam(Rc<Lam>),

    Record(Tele),
    EltCons(Tele),
    ModelCons(Tele),
    Proj(Rc<ModelStx>, Field),

    Block(Tele, Rc<ModelStx>),
}

pub type EltStx = Stx;
pub type TypeStx = Stx;
pub type ModelStx = Stx;
pub type TheoryStx = Stx;

#[derive(Debug)]
pub struct Pi {
    pub args: Tele,
    pub ret: TheoryStx,
}

impl Pi {
    pub fn new(args: Tele, ret: TheoryStx) -> Self {
        Self { args, ret }
    }
}

#[derive(Debug)]
pub struct Lam {
    pub tp: Rc<Pi>, // we keep around the type because we need it for eta-expansion of the result
    pub body: ModelStx,
}

impl Lam {
    pub fn new(tp: Rc<Pi>, body: ModelStx) -> Self {
        Self { tp, body }
    }
}
