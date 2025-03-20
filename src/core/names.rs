use std::fmt;
use ustr::Ustr;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Name(pub Option<Ustr>);

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            Some(n) => write!(f, "{}", n),
            None => write!(f, "_"),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Field(Name, usize);

impl Field {
    pub fn new(name: Name, lvl: usize) -> Self {
        Field(name, lvl)
    }

    pub fn lvl(&self) -> usize {
        self.1
    }

    pub fn name(&self) -> Name {
        self.0
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Idx(Name, usize);

impl Idx {
    pub fn new(name: Name, idx: usize) -> Self {
        Self(name, idx)
    }

    pub fn name(&self) -> Name {
        self.0
    }

    pub fn idx(&self) -> usize {
        self.1
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Lvl(Name, usize);

impl Lvl {
    pub fn new(name: Name, idx: usize) -> Self {
        Self(name, idx)
    }

    pub fn name(&self) -> Name {
        self.0
    }

    pub fn lvl(&self) -> usize {
        self.1
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct TopName(Name, usize);

impl TopName {
    pub fn new(name: Name, lvl: usize) -> Self {
        Self(name, lvl)
    }

    pub fn lvl(&self) -> usize {
        self.1
    }

    pub fn name(&self) -> Name {
        self.0
    }
}
