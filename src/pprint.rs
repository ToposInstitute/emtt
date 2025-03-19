use crate::core::*;
use egg::{Id, RecExpr};
use pretty::RcDoc;

struct PrettyPrinter {
    re: RecExpr<element::Neutral>,
}

impl PrettyPrinter {
    fn pprint_elt<'a>(&'a self, e: &Elt) -> RcDoc<'a> {
        match e {
            Elt::Lvl(_) => panic!("expected an unflattened element"),
            Elt::Id(i) => self.pprint_neu(*i),
            Elt::Erased => RcDoc::text("erased"),
            Elt::Cons(fields) => pretty_util::tuple(fields.iter().map(|e| self.pprint_elt(e))),
        }
    }

    fn pprint_neu(&self, i: Id) -> RcDoc {
        match &self.re[i] {
            element::Neutral::App(f, args, _, flattened) => {
                let f = f.unflatten(flattened);
                let args: Vec<_> = args.iter().map(|arg| arg.unflatten(flattened)).collect();
                todo!()
            }
            element::Neutral::ModelProj(_, _, _, _) => todo!(),
            element::Neutral::Var(_) => todo!(),
            element::Neutral::Proj(_, _) => todo!(),
        }
    }
}
