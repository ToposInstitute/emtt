use super::names::*;
use std::rc::Rc;
use ustr::Ustr;

#[derive(Debug)]
pub enum Telelement {
    Decl(Stx),
    Def(Stx, Stx),
}

impl Telelement {
    pub fn thstx(&self) -> &TheoryStx {
        match self {
            Telelement::Decl(thstx) => thstx,
            Telelement::Def(_, thstx) => thstx,
        }
    }
}

// A telescope consists of a sequence of declarations and definitions.
#[derive(Clone, Debug)]
pub struct Tele(Rc<Vec<(Name, Telelement)>>);

impl Tele {
    pub fn from_vec(v: Vec<(Name, Telelement)>) -> Self {
        Tele(Rc::new(v))
    }

    pub fn iter(&self) -> impl Iterator<Item = &(Name, Telelement)> {
        self.0.iter()
    }

    pub fn num_decls(&self) -> usize {
        self.0
            .iter()
            .filter(|(_, te)| match te {
                Telelement::Decl(_) => true,
                _ => false,
            })
            .count()
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

    pub fn get(&self, field: Field) -> &(Name, Telelement) {
        &self.0[field.lvl()]
    }
}

// A telecons for a telescope just assigns values to the declarations, but can use the definitions
#[derive(Clone, Debug)]
pub struct TeleCons(Rc<Vec<(Name, Stx)>>);

impl TeleCons {
    pub fn from_vec(v: Vec<(Name, Stx)>) -> Self {
        Self(Rc::new(v))
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

#[derive(Clone, Debug)]
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
    EltCons(TeleCons),
    ModelCons(TeleCons),
    Proj(Rc<ModelStx>, Field),

    Block(TeleCons, Rc<ModelStx>),
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
    // we keep around the type because we might need it for eta-expansion of the result?
    // TODO: figure out if this is true
    pub tp: Rc<Pi>,
    pub body: ModelStx,
}

impl Lam {
    pub fn new(tp: Rc<Pi>, body: ModelStx) -> Self {
        Self { tp, body }
    }
}
