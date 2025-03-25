use fexplib::types::*;

use crate::{core::Name, ctx::Ctx, env::Env, eval::Evaluator};

struct Elab {}

enum Telelement<Tm, Tp> {
    Decl(Tp),
    Def(Tm, Tp),
}

struct Tele<Tm, Tp> {
    elts: Vec<(Name, Telelement<Tm, Tp>)>,
}

trait Level {
    type TmStx: Clone;
    type TpStx: Clone;
    type TmVal: Clone;
    type TpVal: Clone;

    fn tp(elab: &Elab, ctx: &mut Ctx, e: &FExp) -> Option<Self::TpVal>;
    fn chk(
        elab: &Elab,
        ctx: &mut Ctx,
        e: &FExp,
        tp: &Self::TpVal,
    ) -> Option<(Self::TmStx, Self::TmVal)>;
    fn syn(elab: &Elab, ctx: &mut Ctx, e: &FExp)
        -> Option<(Self::TmStx, Self::TmVal, Self::TpVal)>;

    fn eval_tm(evaluator: &mut Evaluator, env: &Env, tm: &Self::TmStx) -> Self::TmVal;
    fn eval_tp(evaluator: &mut Evaluator, env: &Env, tm: &Self::TpStx) -> Self::TpVal;

    fn extend_env(env: &Env, val: Self::TmVal) -> Env;
}

impl Elab {
    fn chk_tuple<L: Level>(
        &self,
        ctx: &mut Ctx,
        exprs: &[&FExp],
        tele: Tele<L::TmStx, L::TpStx>,
    ) -> Option<(Vec<L::TmStx>, Vec<L::TmVal>)> {
        if exprs.len() != tele.elts.len() {
            todo!()
            // return error!(
            //     self,
            //     "wrong number of arguments, expected {} got {}",
            //     tele.len(),
            //     exprs.len()
            // );
        }
        let mut stxs = Vec::new();
        let mut vals = Vec::new();
        let mut env = Env::empty();
        let mut ei = exprs.iter();
        for (_, telelement) in tele.elts.iter() {
            let (stx, val) = match telelement {
                Telelement::Decl(tpstx) => {
                    let e = ei.next().unwrap();
                    let tp = L::eval_tp(ctx.evaluator_mut(), &env, tpstx);
                    L::chk(self, ctx, e, &tp)?
                }
                Telelement::Def(tmstx, _) => {
                    (tmstx.clone(), L::eval_tm(ctx.evaluator_mut(), &env, tmstx))
                }
            };
            env = L::extend_env(&env, val.clone());
            stxs.push(stx);
            vals.push(val);
        }
        Some((stxs, vals))
    }

    fn chk_record<L: Level>(
        &self,
        ctx: &mut Ctx,
        mut tele_env: Env,
        exprs: &[&FExp],
        tele: &Tele<L::TmStx, L::TpStx>,
    ) -> Option<(Vec<L::TmStx>, Vec<L::TmVal>)> {
        todo!()
        // if exprs.len() != tele.num_decls() {
        //     return error!(
        //         self,
        //         "wrong number of bindings, expected {} got {}",
        //         tele.len(),
        //         exprs.len()
        //     );
        // }
        // let mut stxs = Vec::new();
        // let mut models = Vec::new();
        // let mut ei = exprs.iter();
        // for (name, te) in tele.iter() {
        //     let (fieldstx, fieldval, th) = match te {
        //         stx::Telelement::Decl(thstx) => {
        //             let e = ei.next().unwrap();
        //             let th = ctx.evaluator_mut().eval_theory(&tele_env, thstx);
        //             let fieldexpr = match e.ast0() {
        //                 App2(L(_, Keyword("=")), fe @ L(_, Var(s)), e) => {
        //                     if Name(Some(ustr(s))) == *name {
        //                         e
        //                     } else {
        //                         return error!(self.at(fe), "unexpected field {}", s);
        //                     }
        //                 }
        //                 _ => return error!(self.at(e), "expected syntax node <var> = <expr>"),
        //             };
        //             let (fieldstx, fieldval) = self.chk_model(ctx, fieldexpr, &th)?;
        //             (fieldstx, fieldval, th)
        //         }
        //         stx::Telelement::Def(modelstx, thstx) => {
        //             let env = ctx.env.clone();
        //             (
        //                 modelstx.clone(),
        //                 ctx.evaluator_mut().eval_model(&env, modelstx),
        //                 ctx.evaluator_mut().eval_theory(&env, thstx),
        //             )
        //         }
        //     };
        //     stxs.push((*name, fieldstx));
        //     models.push(fieldval.clone());
        //     tele_env = tele_env.push(fieldval.clone());
        //     ctx.let_bind(*name, fieldval, &th);
        // }
        // Some((TeleCons::from_vec(stxs), models))
    }
}
