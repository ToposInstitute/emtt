use crate::core::*;
use crate::ctx::*;
use crate::env::Env;
use crate::toplevel::ModelSq;
use crate::toplevel::Sequent;
use crate::toplevel::TheorySq;
use fexplib::types::*;
use std::fmt;
use std::rc::Rc;
use tattle::{declare_error, Loc, Reporter};
use ustr::ustr;
use ustr::Ustr;

pub struct Elaborator {
    reporter: Reporter,
    loc: Option<Loc>,
}

declare_error!(ELAB_ERROR, "elab", "error during elaboration");

macro_rules! error {
    ($elab:expr, $s:literal) => {{
        $elab.error(|f| write!(f, $s))
    }};

    ($elab:expr, $s:literal, $($arg:expr),+) => {{
        $elab.error(|f| write!(f, $s, $($arg),+))
    }};
}

impl Elaborator {
    pub fn new(reporter: Reporter) -> Self {
        Self {
            reporter,
            loc: None,
        }
    }

    fn error<T, F: Fn(&mut fmt::Formatter) -> fmt::Result>(&self, f: F) -> Option<T> {
        self.reporter.error_option_loc(self.loc, ELAB_ERROR, f);
        None
    }

    fn at(&self, e: &FExp) -> Self {
        Self {
            reporter: self.reporter.clone(),
            loc: Some(e.loc()),
        }
    }

    fn tp(&self, ctx: &mut Ctx, e: &FExp) -> Option<(TypeStx, Type)> {
        let (stx, th) = self.chk_model(ctx, e, &Theory::Tp)?;
        Some((stx, th.as_type()))
    }

    fn chk_model_tuple(
        &self,
        ctx: &mut Ctx,
        exprs: &[&FExp],
        tele: &Tele,
    ) -> Option<(Vec<ModelStx>, Vec<Model>)> {
        if exprs.len() != tele.len() {
            return error!(
                self,
                "wrong number of arguments, expected {} got {}",
                tele.len(),
                exprs.len()
            );
        }
        let mut stxs = Vec::new();
        let mut models = Vec::new();
        let mut env = Env::empty();
        for (e, (_, thstx)) in exprs.iter().zip(tele.iter()) {
            let th = ctx.evaluator_mut().eval_theory(&env, thstx);
            let (modelstx, model) = self.chk_model(ctx, e, &th)?;
            env = env.push(model.clone());
            stxs.push(modelstx);
            models.push(model);
        }
        Some((stxs, models))
    }

    fn chk_elt_tuple(
        &self,
        ctx: &mut Ctx,
        mut env: Env,
        exprs: &[&FExp],
        tele: &Tele,
    ) -> Option<(Vec<EltStx>, Vec<Elt>, Env)> {
        if exprs.len() != tele.len() {
            return error!(
                self,
                "wrong number of arguments, expected {} got {}",
                tele.len(),
                exprs.len()
            );
        }
        let mut stxs = Vec::new();
        let mut elts = Vec::new();
        for (e, (_, tpstx)) in exprs.iter().zip(tele.iter()) {
            let tp = ctx.evaluator_mut().eval_type(&env, tpstx);
            let (eltstx, elt) = self.chk_elt(ctx, e, &tp)?;
            env = env.push(Model::Elt(elt.clone()));
            stxs.push(eltstx);
            elts.push(elt);
        }
        Some((stxs, elts, env))
    }

    // this modifies ctx, so one must checkpoint and restore afterwards
    // However, we don't *intro* anything, so we don't need to checkpoint the cron
    fn chk_model_tele(
        &self,
        ctx: &mut Ctx,
        mut tele_env: Env,
        exprs: &[&FExp],
        tele: &Tele,
    ) -> Option<(Tele, Vec<Model>)> {
        if exprs.len() != tele.len() {
            return error!(
                self,
                "wrong number of bindings, expected {} got {}",
                tele.len(),
                exprs.len()
            );
        }
        let mut stxs = Vec::new();
        let mut models = Vec::new();
        for (e, (name, thstx)) in exprs.iter().zip(tele.iter()) {
            let th = ctx.evaluator_mut().eval_theory(&tele_env, thstx);
            let fieldexpr = match e.ast0() {
                App2(L(_, Keyword("=")), fe @ L(_, Var(s)), e) => {
                    if Name(Some(ustr(s))) == *name {
                        e
                    } else {
                        return error!(self.at(fe), "unexpected field {}", s);
                    }
                }
                _ => return error!(self.at(e), "expected syntax node <var> = <expr>"),
            };
            let (fieldstx, fieldval) = self.chk_model(ctx, fieldexpr, &th)?;
            stxs.push((*name, fieldstx));
            models.push(fieldval.clone());
            tele_env = tele_env.push(fieldval.clone());
            ctx.let_bind(*name, fieldval, &th);
        }
        Some((Tele::from_vec(stxs), models))
    }

    fn chk_elt_tele(
        &self,
        ctx: &mut Ctx,
        mut tele_env: Env,
        exprs: &[&FExp],
        tele: &Tele,
    ) -> Option<(Tele, Vec<Elt>)> {
        if exprs.len() != tele.len() {
            return error!(
                self,
                "wrong number of bindings, expected {} got {}",
                tele.len(),
                exprs.len()
            );
        }
        let mut stxs = Vec::new();
        let mut elts = Vec::new();
        for (e, (name, tpstx)) in exprs.iter().zip(tele.iter()) {
            let tp = ctx.evaluator_mut().eval_type(&tele_env, tpstx);
            let fieldexpr = match e.ast0() {
                App2(L(_, Keyword("=")), fe @ L(_, Var(s)), e) => {
                    if Name(Some(ustr(s))) == *name {
                        e
                    } else {
                        return error!(self.at(fe), "unexpected field {}", s);
                    }
                }
                _ => return error!(self.at(e), "expected syntax node <var> = <expr>"),
            };
            let (fieldstx, fieldval) = self.chk_elt(ctx, fieldexpr, &tp)?;
            stxs.push((*name, fieldstx));
            elts.push(fieldval.clone());
            tele_env = tele_env.push(Model::Elt(fieldval.clone()));
            ctx.let_bind(*name, Model::Elt(fieldval), &Theory::Type(tp));
        }
        Some((Tele::from_vec(stxs), elts))
    }

    // this intros variables, so we must reset the context afterwards
    fn syn_tp_tele(&self, ctx: &mut Ctx, exprs: &[&FExp]) -> Option<Tele> {
        let mut stxs = Vec::new();
        for e in exprs {
            let (name, tpexpr) = match e.ast0() {
                App2(L(_, Keyword(":")), L(_, Var(s)), tpexpr) => (Name(Some(ustr(s))), tpexpr),
                _ => (Name(None), e),
            };
            let (tpstx, tp) = self.tp(ctx, tpexpr)?;
            ctx.intro_elt(name, &tp);
            stxs.push((name, tpstx));
        }
        Some(Tele::from_vec(stxs))
    }

    fn intro_elt(&self, ctx: &mut Ctx, binding: &FExp) -> Option<(Name, TypeStx)> {
        let (name, tpexpr) = match binding.ast0() {
            App2(L(_, Keyword(":")), L(_, Var(s)), tpexpr) => (Name(Some(ustr(s))), *tpexpr),
            _ => (Name(None), binding),
        };
        let (tpstx, tp) = self.tp(ctx, tpexpr)?;
        ctx.intro_elt(name, &tp);
        Some((name, tpstx))
    }

    fn intro_elts(&self, ctx: &mut Ctx, bindings: &[&FExp]) -> Option<Tele> {
        let mut v = Vec::with_capacity(bindings.len());
        for binding in bindings.iter() {
            v.push(self.intro_elt(ctx, binding)?);
        }
        Some(Tele::from_vec(v))
    }

    fn pi_theory(&self, ctx: &mut Ctx, args: &[&FExp], body: &FExp) -> Option<(TheoryStx, Theory)> {
        let orig_env = ctx.env.clone();
        let arg_tele = self.intro_elts(ctx, args)?;
        let (bodystx, _) = self.theory(ctx, body)?;
        let pi = Rc::new(Pi::new(arg_tele, bodystx));
        Some((Stx::Pi(pi.clone()), Theory::Pi(orig_env, pi)))
    }

    fn lookup(&self, ctx: &Ctx, name: Ustr) -> Option<Binding> {
        ctx.lookup(name)
            .or_else(|| error!(self, "could not find variable {}", name.as_str()))
    }

    fn theory_app(
        &self,
        ctx: &mut Ctx,
        s: &str,
        args: Option<&[&FExp]>,
    ) -> Option<(TheoryStx, Theory)> {
        let tn = ctx.topname(ustr(s))?;
        let thdecl = ctx.toplevel().try_theory(tn)?;
        let (argstxs, argmodels) = match (thdecl.args.as_ref(), args) {
            (Some(declargs), Some(args)) => self.chk_model_tuple(ctx, args, declargs)?,
            (None, None) => (vec![], vec![]),
            (Some(_), None) => return error!(self, "expected arguments, found none"),
            (None, Some(_)) => return error!(self, "expected no arguments, found some"),
        };
        Some((
            Stx::TopApp(tn, argstxs),
            Theory::TopApp(tn, Rc::new(argmodels)),
        ))
    }

    fn real_theory(&self, ctx: &mut Ctx, e: &FExp) -> Option<(TheoryStx, Theory)> {
        match e.ast0() {
            Prim("type") => Some((Stx::Tp, Theory::Tp)),
            App2(L(_, Keyword("->")), L(_, Tuple(args)), body) => {
                let c = ctx.checkpoint();
                let ret = self.pi_theory(ctx, args, body);
                ctx.return_to(c);
                ret
            }
            Var(s) => self.at(e).theory_app(ctx, s, None),
            App1(L(_, Var(s)), L(_, Tuple(args))) => self.at(e).theory_app(ctx, s, Some(&args)),
            _ => None,
        }
    }

    fn theory(&self, ctx: &mut Ctx, e: &FExp) -> Option<(TheoryStx, Theory)> {
        // try to get a real theory out, otherwise fall back to type
        match self.real_theory(ctx, e) {
            Some(t) => Some(t),
            None => {
                let (tpstx, tp) = self.tp(ctx, e)?;
                Some((tpstx, Theory::Type(tp)))
            }
        }
    }

    fn chk_lam(
        &self,
        ctx: &mut Ctx,
        th: &Theory,
        args: &[&FExp],
        body: &FExp,
    ) -> Option<(ModelStx, Model)> {
        let orig_env = ctx.env.clone();
        let (mut pienv, pi) = match th {
            Theory::Pi(env, pi) => (env.clone(), pi.clone()),
            _ => return error!(self, "can only check lam against pi theory"),
        };
        if args.len() != pi.args.len() {
            return error!(
                self,
                "expected {} bindings in head of lam, got {}",
                pi.args.len(),
                args.len()
            );
        }
        for (argexpr, (_, tpstx)) in args.iter().zip(pi.args.iter()) {
            let name = match argexpr.ast0() {
                Var("_") => Name(None),
                Var(s) => Name(Some(ustr(s))),
                _ => return error!(self.at(argexpr), "expected variable or underscore"),
            };
            let tp = ctx.evaluator_mut().eval_type(&pienv, tpstx);
            let e = ctx.intro_elt(name, &tp);
            pienv = pienv.push(Model::Elt(e));
        }
        let retth = ctx.evaluator_mut().eval_theory(&pienv, &pi.ret);
        let (bodystx, _) = self.chk_model(ctx, body, &retth)?;
        let lam = Rc::new(Lam::new(pi.clone(), bodystx));
        Some((ModelStx::Lam(lam.clone()), Model::Lam(orig_env, lam)))
    }

    fn model_app(
        &self,
        ctx: &mut Ctx,
        headval: Model,
        headstx: Stx,
        headth: &Theory,
        args: &[&FExp],
    ) -> Option<(ModelStx, Model, Theory)> {
        let (env, pi) = match headth {
            Theory::Pi(env, pi) => (env, pi),
            _ => return error!(self, "cannot apply non-pi type"),
        };
        let (argstxs, args, env) = self.chk_elt_tuple(ctx, env.clone(), args, &pi.args)?;
        let retth = ctx.evaluator_mut().eval_theory(&env, &pi.ret);
        if retth.is_type() {
            let ret = headval.elt_app(ctx.evaluator_mut(), args);
            Some((
                Stx::EltApp(Rc::new(headstx), argstxs),
                Model::Elt(ret),
                retth,
            ))
        } else {
            let ret = headval.model_app(ctx.evaluator_mut(), args);
            Some((Stx::ModelApp(Rc::new(headstx), argstxs), ret, retth))
        }
    }

    fn syn_model(&self, ctx: &mut Ctx, e: &FExp) -> Option<(ModelStx, Model, Theory)> {
        match e.ast0() {
            Var(s) => match self.at(e).lookup(ctx, ustr(s))? {
                Binding::Local(l, th) => {
                    let i = ctx.lvl_to_idx(l);
                    Some((Stx::Var(i), ctx.env.get(i), th))
                }
                Binding::Toplevel(tn) => {
                    let Some(modelsequent) = ctx.toplevel().try_model(tn) else {
                        return error!(
                            self.at(e),
                            "expected {} to refer to a model sequent",
                            tn.name()
                        );
                    };
                    let None = modelsequent.args.as_ref() else {
                        return error!(
                            self.at(e),
                            "expected {} to refer to a model sequent with no args",
                            tn.name()
                        );
                    };
                    let env = Env::empty();
                    let theory = ctx.evaluator_mut().eval_theory(&env, &modelsequent.theory);
                    let model = ctx.evaluator_mut().eval_model(&env, &modelsequent.body);
                    Some((Stx::TopApp(tn, vec![]), model, theory))
                }
            },
            App1(h @ L(_, Var(s)), L(_, Tuple(args))) => match self.at(h).lookup(ctx, ustr(s))? {
                Binding::Toplevel(tn) => {
                    let Some(modelsequent) = ctx.toplevel().try_model(tn) else {
                        return error!(self.at(h), "expected {} to refer to a model sequent", s);
                    };
                    let Some(declargs) = modelsequent.args.as_ref() else {
                        return error!(self.at(h), "expected {} to take arguments", s);
                    };
                    let (argstxs, argmodels) =
                        self.chk_model_tuple(ctx, args.as_slice(), declargs)?;
                    let env: Env = argmodels.into_iter().into();
                    let theory = ctx.evaluator_mut().eval_theory(&env, &modelsequent.theory);
                    let model = ctx.evaluator_mut().eval_model(&env, &modelsequent.body);
                    Some((Stx::TopApp(tn, argstxs), model, theory))
                }
                Binding::Local(l, th) => {
                    let i = ctx.lvl_to_idx(l);
                    let headstx = Stx::Var(i);
                    let headval = ctx.env.get(i);
                    self.at(h).model_app(ctx, headval, headstx, &th, args)
                }
            },
            App1(h, L(_, Tuple(args))) => {
                let (headstx, headval, headth) = self.syn_model(ctx, h)?;
                self.at(h).model_app(ctx, headval, headstx, &headth, args)
            }
            App1(h, fe @ L(_, Field(f))) => {
                let (hstx, hval, hth) = self.syn_model(ctx, h)?;
                match hth {
                    Theory::TopApp(tn, args) => {
                        let theorysq = ctx.toplevel().theory(tn);
                        let Some(field) = theorysq.body.find_field(ustr(f)) else {
                            return error!(self.at(fe), "no such field");
                        };
                        let env: Env = args.iter().cloned().into();
                        let (env, val) = match hval {
                            Model::Cons(fields) => (
                                env.extend(fields.iter().cloned().take(field.lvl())),
                                fields[field.lvl()].clone(),
                            ),
                            _ => panic!("expected values of record type to be eta-expanded"),
                        };
                        let th = ctx
                            .evaluator_mut()
                            .eval_theory(&env, &theorysq.body.get(field).1);
                        Some((Stx::Proj(Rc::new(hstx), field), val, th))
                    }
                    Theory::Type(Type::Record(env, tele)) => {
                        let Some(field) = tele.find_field(ustr(f)) else {
                            return error!(self.at(fe), "no such field");
                        };
                        let (env, val) = match hval {
                            Model::Elt(Elt::Cons(fields)) => (
                                env.extend(fields.iter().cloned().take(field.lvl())),
                                fields[field.lvl()].clone(),
                            ),
                            _ => panic!("expected values of record type to be eta-expanded"),
                        };
                        let tp = ctx.evaluator_mut().eval_type(&env, &tele.get(field).1);
                        Some((
                            Stx::Proj(Rc::new(hstx), field),
                            Model::Elt(val),
                            Theory::Type(tp),
                        ))
                    }
                    _ => error!(
                        self.at(e),
                        "can only take projections of models or elements of record type"
                    ),
                }
            }
            App2(L(_, Keyword("==")), le, re) => {
                let (lstx, l, tp) = self.syn_elt(ctx, le)?;
                let (rstx, r) = self.chk_elt(ctx, re, &tp)?;
                Some((
                    Stx::Equals(Rc::new(lstx), Rc::new(rstx)),
                    Model::Type(Type::Equals(l, r)),
                    Theory::Tp,
                ))
            }
            Block(bindings, Some(ret)) => {
                let c = ctx.checkpoint_lite();
                let ret = self.syn_model_let(ctx, bindings, ret);
                ctx.return_to_lite(c);
                ret
            }
            App2(L(_, Keyword("↦")), _, _) => error!(self.at(e), "must check lambdas"),
            Block(_, None) => error!(self.at(e), "must check model records"),
            _ => error!(
                self.at(e),
                "unexpected syntax node for synthesizing elaboration"
            ),
        }
    }

    fn syn_model_let(
        &self,
        ctx: &mut Ctx,
        bindings: &[&FExp],
        retexp: &FExp,
    ) -> Option<(ModelStx, Model, Theory)> {
        let mut stxs = Vec::new();
        for binding in bindings.iter() {
            let (name, valexpr) = match binding.ast0() {
                App2(L(_, Keyword("=")), L(_, Var(s)), e) => (Name(Some(ustr(s))), e),
                _ => (Name(None), binding),
            };
            let (valstx, val, th) = self.syn_model(ctx, valexpr)?;
            stxs.push((name, valstx));
            ctx.let_bind(name, val, &th);
        }
        let (retstx, ret, retth) = self.syn_model(ctx, retexp)?;
        Some((
            Stx::Block(Tele::from_vec(stxs), Rc::new(retstx)),
            ret,
            retth,
        ))
    }

    fn chk_model_let(
        &self,
        ctx: &mut Ctx,
        bindings: &[&FExp],
        retexp: &FExp,
        th: &Theory,
    ) -> Option<(ModelStx, Model)> {
        let mut stxs = Vec::new();
        for binding in bindings.iter() {
            let (name, valexpr) = match binding.ast0() {
                App2(L(_, Keyword("=")), L(_, Var(s)), e) => (Name(Some(ustr(s))), e),
                _ => (Name(None), binding),
            };
            let (valstx, val, th) = self.syn_model(ctx, valexpr)?;
            stxs.push((name, valstx));
            ctx.let_bind(name, val, &th);
        }
        let (retstx, ret) = self.chk_model(ctx, retexp, th)?;
        Some((Stx::Block(Tele::from_vec(stxs), Rc::new(retstx)), ret))
    }

    fn syn_elt(&self, ctx: &mut Ctx, e: &FExp) -> Option<(EltStx, Elt, Type)> {
        let (stx, m, th) = self.syn_model(ctx, e)?;
        let (e, tp) = match (m, th) {
            (Model::Elt(e), Theory::Type(tp)) => (e, tp),
            _ => return error!(self.at(e), "expected element, got model"),
        };
        Some((stx, e, tp))
    }

    fn chk_model(&self, ctx: &mut Ctx, e: &FExp, th: &Theory) -> Option<(ModelStx, Model)> {
        match e.ast0() {
            Prim("refl") => match th {
                Theory::Type(Type::Equals(e1, e2)) => {
                    if ctx.convertable_elts(e1, e2) {
                        Some((Stx::Refl, Model::Elt(Elt::Erased)))
                    } else {
                        error!(self.at(e), "could not unify the sides of this equation")
                    }
                }
                _ => error!(self.at(e), "tried to check refl against non-equality type"),
            },
            Block(bindings, None) => match th {
                Theory::Type(Type::Record(env, fields)) => {
                    let env = env.clone();
                    let c = ctx.checkpoint_lite();
                    let ret = self.chk_elt_tele(ctx, env, bindings, fields);
                    ctx.return_to_lite(c);
                    let (telestx, elts) = ret?;
                    Some((Stx::EltCons(telestx), Model::Elt(Elt::Cons(Rc::new(elts)))))
                }
                Theory::TopApp(tn, args) => {
                    let theorysq = ctx.toplevel().theory(*tn);
                    let env: Env = args.iter().cloned().into();
                    let c = ctx.checkpoint_lite();
                    let ret = self.chk_model_tele(ctx, env, bindings, &theorysq.body);
                    ctx.return_to_lite(c);
                    let (telestx, models) = ret?;
                    Some((Stx::ModelCons(telestx), Model::Cons(Rc::new(models))))
                }
                Theory::Tp => {
                    let c = ctx.checkpoint();
                    let ret = self.syn_tp_tele(ctx, bindings);
                    ctx.return_to(c);
                    let tptele = ret?;
                    Some((
                        Stx::Record(tptele.clone()),
                        Model::Type(Type::Record(ctx.env.clone(), tptele)),
                    ))
                }
                _ => error!(
                    self.at(e),
                    "tried to check record syntax against non-record type"
                ),
            },
            App2(L(_, Keyword("↦")), L(_, Tuple(args)), bodyexpr) => {
                let c = ctx.checkpoint();
                let ret = self.chk_lam(ctx, th, args, bodyexpr);
                ctx.return_to(c);
                ret
            }
            Block(bindings, Some(ret)) => {
                let c = ctx.checkpoint_lite();
                let ret = self.chk_model_let(ctx, bindings, ret, th);
                ctx.return_to_lite(c);
                ret
            }
            _ => {
                let (stx, m, synthedth) = self.syn_model(ctx, e)?;
                if ctx.convertable_theories(th, &synthedth) {
                    Some((stx, m))
                } else {
                    error!(
                        self.at(e),
                        "synthesized type did not match type checked against"
                    )
                }
            }
        }
    }

    fn chk_elt(&self, ctx: &mut Ctx, e: &FExp, tp: &Type) -> Option<(EltStx, Elt)> {
        let (stx, m) = self.chk_model(ctx, e, &Theory::Type(tp.clone()))?;
        Some((stx, m.as_elt()))
    }

    pub fn syn_th_tele(&self, ctx: &mut Ctx, bindings: &[&FExp]) -> Option<Tele> {
        let mut stxs = Vec::new();
        for binding in bindings {
            let (name, thexpr) = match binding.ast0() {
                App2(L(_, Keyword(":")), L(_, Var(s)), thexpr) => (Name(Some(ustr(s))), thexpr),
                _ => return error!(self.at(binding), "expected binding <var> : <theory>"),
            };
            let (thstx, th) = self.theory(ctx, thexpr)?;
            stxs.push((name, thstx));
            ctx.intro_model(name, &th);
        }
        Some(Tele::from_vec(stxs))
    }

    fn theory_sequent(
        &self,
        ctx: &mut Ctx,
        argexprs: Option<&[&FExp]>,
        bodye: &FExp,
    ) -> Option<TheorySq> {
        let args = match argexprs {
            Some(argexprs) => Some(self.syn_th_tele(ctx, argexprs)?),
            None => None,
        };
        let body = match bodye.ast0() {
            Block(bindings, None) => self.syn_th_tele(ctx, bindings)?,
            _ => return error!(self.at(bodye), "expected record"),
        };
        Some(TheorySq::new(args, body))
    }

    fn model_sequent(
        &self,
        ctx: &mut Ctx,
        argexprs: Option<&[&FExp]>,
        retthe: &FExp,
        bodye: &FExp,
    ) -> Option<ModelSq> {
        let args = match argexprs {
            Some(argexprs) => Some(self.syn_th_tele(ctx, argexprs)?),
            None => None,
        };
        let (retthstx, retth) = self.theory(ctx, retthe)?;
        let (bodystx, _) = self.chk_model(ctx, bodye, &retth)?;
        Some(ModelSq::new(args, retthstx, bodystx))
    }

    pub fn sequent(&self, ctx: &mut Ctx, e: &FExp) -> Option<(Ustr, Sequent)> {
        let (heade, bodye) = match e.ast0() {
            App2(L(_, Keyword("=")), heade, bodye) => (heade, bodye),
            _ => return error!(self.at(e), "expected sequent"),
        };
        let (name, argexprs, kindexpr) = match heade.ast0() {
            App2(L(_, Keyword(":")), head, kindexpr) => {
                let (s, argexprs) = match head.ast0() {
                    Var(s) => (s, None),
                    App1(L(_, Var(s)), L(_, Tuple(argexprs))) => (s, Some(argexprs.as_slice())),
                    _ => return error!(self.at(head), "expected <var> or <var>[<args>...]"),
                };
                (ustr(s), argexprs, kindexpr)
            }
            _ => {
                return error!(
                    self.at(e),
                    "expected sequent head of the form <var>[<args>...] : <kind>"
                )
            }
        };
        match kindexpr.ast0() {
            Prim("theory") => Some((
                name,
                Sequent::Theory(self.theory_sequent(ctx, argexprs, bodye)?),
            )),
            _ => Some((
                name,
                Sequent::Model(self.model_sequent(ctx, argexprs, kindexpr, bodye)?),
            )),
        }
    }
}
