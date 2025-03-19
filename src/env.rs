use crate::{core::*, cron::CRoN, toplevel::Toplevel};
use bwd::Bwd;
use egg::EGraph;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct Env {
    values: Bwd<Model>,
    len: usize,
}

impl Env {
    pub fn len(&self) -> usize {
        self.len
    }

    pub fn get(&self, i: Idx) -> Model {
        self.values.get(i.idx()).unwrap().clone()
    }

    pub fn empty() -> Self {
        Env {
            values: Bwd::nil(),
            len: 0,
        }
    }

    pub fn push(&self, m: Model) -> Self {
        Self {
            values: self.values.snoc(m),
            len: self.len + 1,
        }
    }

    pub fn extend<T: Into<Model> + Sized, I: Iterator<Item = T> + ExactSizeIterator>(
        &self,
        iter: I,
    ) -> Env {
        let k = iter.len();
        Self {
            values: iter.fold(self.values.clone(), |vs, v| vs.snoc(v.into())),
            len: self.len + k,
        }
    }
}

impl<I: Iterator<Item = Model> + ExactSizeIterator, L: IntoIterator<IntoIter = I>> From<L> for Env {
    fn from(v: L) -> Self {
        Env::empty().extend(v.into_iter())
    }
}
