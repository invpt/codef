use crate::reifier::{self, Builtin, VariantItemType};

use rustc_hash::FxHashMap;

mod lir;
pub use lir::*;

pub fn lower<'s>(module: &reifier::Module<'s>) -> Module<'s> {
    todo!()
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct LabelRef(usize);

#[derive(Debug)]
struct LinearTacLowerer<'a> {
    args: Vec<TempRef>,
    temps: Vec<TempInfo>,
    labels: Vec<Option<BlockRef>>,
    blocks: Vec<(Vec<Insn>, Option<Branch<LabelRef>>, Ctrl<LabelRef>)>,
    current_insns: Vec<Insn>,
    current_branch: Option<Branch<LabelRef>>,
    current_ctrl: Option<Ctrl<LabelRef>>,

    locals: FxHashMap<reifier::Symbol, TempRef>,
    variant_ids: FxHashMap<reifier::VariantItemType<'a>, u64>,
    variant_id_counter: u64,
    reified_module: &'a reifier::Module<'a>,
}

impl<'a> LinearTacLowerer<'a> {
    fn new(reified_module: &'a reifier::Module<'a>) -> LinearTacLowerer<'a> {
        LinearTacLowerer {
            args: Vec::new(),
            temps: Vec::new(),
            labels: Vec::new(),
            blocks: Vec::new(),
            current_insns: Vec::new(),
            current_branch: None,
            current_ctrl: None,
            locals: FxHashMap::default(),
            variant_ids: FxHashMap::default(),
            variant_id_counter: 0,
            reified_module,
        }
    }

    fn lower(
        mut self,
        spec: bool,
        arg: Option<&reifier::Pattern<'a>>,
        body: &reifier::Expr<'a>,
    ) -> Cfg {
        if let Some(arg) = arg {
            if let reifier::PatternKind::Tuple(items) = &arg.kind {
                for item in &**items {
                    let temp = self.new_temp(Kind::of(&item.ty));
                    self.args.push(temp);
                    self.abstract_arg(item, temp);
                }
            } else {
                let temp = self.new_temp(Kind::of(&arg.ty));
                self.args.push(temp);
                self.abstract_arg(arg, temp);
            }
        }

        if !spec {
            let ret = self.expr(body, true).unwrap();
            self.ctrl(Ctrl::Return(ret));
        } else {
            // 1. pack all the args into one tuple
            let reifying_args = self.new_temp(Kind::Integer);
            self.alloc(reifying_args, 8 * self.args.len() as u64);
            for (i, &arg) in self.args.clone().iter().enumerate() {
                self.insn(Insn::Store(reifying_args, arg, i as u64 * 8))
            }

            let reifying_args_pat = arg.expect("Cannot reify with empty arguments");

            // if this is a spec, the body must be an immediate lambda
            let reifier::ExprKind::Abstract { spec, arg, body } = &body.kind else {
                panic!("Cannot specialize non-function body")
            };

            let mut lowerer = Self::new(self.reified_module);

            // the inner proc gets one extra initial argument: the tuple of all the reified stuff
            let arg_temp = lowerer.new_temp(Kind::Integer);
            lowerer.args.push(arg_temp);
            lowerer.abstract_arg(reifying_args_pat, arg_temp);

            let proc = lowerer.lower(*spec, arg.as_ref(), &body);

            let proc_temp = self.new_temp(Kind::Integer);
            let spec_temp = self.new_temp(Kind::Integer);
            let spec_res_temp = self.new_temp(Kind::Integer);
            self.insn(Insn::LoadIr(proc_temp, proc));
            self.insn(Insn::LoadBuiltin(spec_temp, Builtin::Spec));
            self.insn(Insn::Call(
                spec_res_temp,
                spec_temp,
                Box::new([proc_temp, arg_temp]),
            ));
            self.ctrl(Ctrl::Return(spec_res_temp))
        }

        let mut blocks = Vec::new();
        for (insns, branch, ctrl) in self.blocks {
            blocks.push(Block {
                params: Box::new([]),
                insns: insns.into_boxed_slice(),
                branch: if let Some(Branch(cmp, a, b, t)) = branch {
                    Some(Branch(
                        cmp,
                        a,
                        b,
                        Target {
                            arguments: Box::new([]),
                            block: self.labels[t.0].expect("label target unset"),
                        },
                    ))
                } else {
                    None
                },
                ctrl: match ctrl {
                    Ctrl::Jump(t) => Ctrl::Jump(Target {
                        arguments: Box::new([]),
                        block: self.labels[t.0].expect("label target unset"),
                    }),
                    Ctrl::Return(r) => Ctrl::Return(r),
                },
            })
        }

        Cfg {
            args: self.args.into_boxed_slice(),
            temps: self.temps.into_boxed_slice(),
            blocks: blocks.into_boxed_slice(),
        }
    }

    fn expr(&mut self, expr: &reifier::Expr<'a>, want_output: bool) -> Option<TempRef> {
        match &expr.kind {
            reifier::ExprKind::Scope(scope) => {
                let len = scope.exprs.len();
                for (i, expr) in scope.exprs.iter().enumerate() {
                    if i == len - 1 && !scope.discard {
                        return self.expr(expr, want_output);
                    } else {
                        self.expr(expr, false);
                    }
                }

                if want_output {
                    let empty = self.new_temp(Kind::Integer);
                    self.alloc(empty, 0);
                    Some(empty)
                } else {
                    None
                }
            }
            reifier::ExprKind::Abstract { .. } => todo!("general closures not currently supported"),
            reifier::ExprKind::For {
                init,
                cond,
                afterthought,
                body,
            } => {
                if let Some(init) = init {
                    self.expr(init, false);
                }
                let cond_lab = self.new_label();
                let body_lab = self.new_label();
                self.ctrl(Ctrl::Jump(cond_lab));
                self.set_label_target(body_lab);
                self.expr(body, false);
                if let Some(afterthought) = afterthought {
                    self.expr(afterthought, false);
                }
                self.set_label_target(cond_lab);
                self.cond(&cond, JumpCond::True(body_lab));

                if want_output {
                    let empty = self.new_temp(Kind::Integer);
                    self.alloc(empty, 0);
                    Some(empty)
                } else {
                    None
                }
            }
            reifier::ExprKind::Case {
                cond,
                on_true,
                on_false,
            } => {
                let on_false_lab = self.new_label();
                let done_lab = self.new_label();
                let res = self.new_temp(Kind::of(&expr.ty));
                self.cond(cond, JumpCond::False(on_false_lab));
                let true_res = self
                    .expr(on_true, want_output && on_false.is_some())
                    .unwrap();
                if want_output && on_false.is_some() {
                    self.insn(Insn::Copy(res, true_res));
                }
                self.ctrl(Ctrl::Jump(done_lab));
                self.set_label_target(on_false_lab);
                if let Some(on_false) = on_false {
                    let false_res = self.expr(on_false, want_output).unwrap();
                    self.insn(Insn::Copy(res, false_res));
                    self.set_label_target(done_lab);
                } else {
                    self.set_label_target(done_lab);
                }

                if want_output {
                    if on_false.is_some() {
                        Some(res)
                    } else {
                        let empty = self.new_temp(Kind::Integer);
                        self.alloc(empty, 0);
                        Some(empty)
                    }
                } else {
                    None
                }
            }
            reifier::ExprKind::Tuple(items) => {
                let out = self.new_temp(Kind::Integer);
                self.alloc(out, items.len() as u64 * 8);
                for (i, item) in items.iter().enumerate() {
                    let item_temp = self.expr(item, true).unwrap();
                    self.insn(Insn::Store(out, item_temp, i as u64 * 8));
                }

                if want_output {
                    Some(out)
                } else {
                    None
                }
            }
            reifier::ExprKind::StructuralEq(pat, expr) => {
                let val = self.expr(expr, true).unwrap();
                let fail_lab = self.new_label();
                let end_lab = self.new_label();
                let out = self.new_temp(Kind::Integer);
                let mut assignments = Vec::new();
                self.structural_eq(pat, val, fail_lab, &mut assignments);
                for (sym, temp) in assignments {
                    self.insn(Insn::Copy(*self.locals.get(&sym).unwrap(), temp));
                }
                if want_output {
                    self.insn(Insn::ConstI(out, 1));
                    self.ctrl(Ctrl::Jump(end_lab));
                }
                self.set_label_target(fail_lab);
                if want_output {
                    self.insn(Insn::ConstI(out, 0));
                    self.set_label_target(end_lab);
                }

                if want_output {
                    Some(out)
                } else {
                    None
                }
            }
            reifier::ExprKind::Binary(reifier::BinOp::And, a, b) => {
                let lab = self.new_label();
                let endlab = self.new_label();
                let out = self.new_temp(Kind::Integer);
                let zero = self.new_temp(Kind::Integer);
                self.insn(Insn::ConstI(zero, 0));
                let a = self.expr(a, true).unwrap();
                self.branch(Branch(BranchCmp::Neq, a, zero, lab));
                self.insn(Insn::Copy(out, a));
                self.ctrl(Ctrl::Jump(endlab));
                self.set_label_target(lab);
                let b = self.expr(b, true).unwrap();
                self.insn(Insn::Copy(out, b));
                self.set_label_target(endlab);

                if want_output {
                    Some(out)
                } else {
                    None
                }
            }
            reifier::ExprKind::Binary(reifier::BinOp::Or, a, b) => {
                let lab = self.new_label();
                let endlab = self.new_label();
                let out = self.new_temp(Kind::Integer);
                let zero = self.new_temp(Kind::Integer);
                self.insn(Insn::ConstI(zero, 0));
                let a = self.expr(a, true).unwrap();
                self.branch(Branch(BranchCmp::Eq, a, zero, lab));
                self.insn(Insn::Copy(out, a));
                self.ctrl(Ctrl::Jump(endlab));
                self.set_label_target(lab);
                let b = self.expr(b, true).unwrap();
                self.insn(Insn::Copy(out, b));
                self.set_label_target(endlab);

                if want_output {
                    Some(out)
                } else {
                    None
                }
            }
            reifier::ExprKind::Binary(_, a, b) if !want_output => {
                self.expr(a, false);
                self.expr(b, false);
                None
            }
            reifier::ExprKind::Binary(op, a, b) => {
                let a_ty = &a.ty;
                let a = self.expr(a, true).unwrap();
                let b = self.expr(b, true).unwrap();
                let out = self.new_temp(Kind::of(&expr.ty));

                let insn = match op {
                    reifier::BinOp::Add if expr.ty.is_int() => Insn::AddI(out, a, b),
                    reifier::BinOp::Sub if expr.ty.is_int() => Insn::SubI(out, a, b),
                    reifier::BinOp::Mul if expr.ty.is_int() => Insn::MulI(out, a, b),
                    reifier::BinOp::Div if expr.ty.is_int() => Insn::DivI(out, a, b),
                    reifier::BinOp::Add if expr.ty.is_float() => Insn::AddF(out, a, b),
                    reifier::BinOp::Sub if expr.ty.is_float() => Insn::SubF(out, a, b),
                    reifier::BinOp::Mul if expr.ty.is_float() => Insn::MulF(out, a, b),
                    reifier::BinOp::Div if expr.ty.is_float() => Insn::DivF(out, a, b),
                    reifier::BinOp::Add
                    | reifier::BinOp::Sub
                    | reifier::BinOp::Mul
                    | reifier::BinOp::Div => panic!("Unexpected type for Add/Sub/Mul/Div"),
                    reifier::BinOp::Or | reifier::BinOp::And => unreachable!(),
                    reifier::BinOp::Eq if a_ty.is_int() => Insn::EqI(out, a, b),
                    reifier::BinOp::Neq if a_ty.is_int() => Insn::NeqI(out, a, b),
                    reifier::BinOp::Lt if a_ty.is_int() => Insn::LtI(out, a, b),
                    reifier::BinOp::Leq if a_ty.is_int() => Insn::LeqI(out, a, b),
                    reifier::BinOp::Gt if a_ty.is_int() => Insn::LtI(out, b, a),
                    reifier::BinOp::Geq if a_ty.is_int() => Insn::LeqI(out, b, a),
                    reifier::BinOp::Eq if a_ty.is_float() => Insn::EqF(out, a, b),
                    reifier::BinOp::Neq if a_ty.is_float() => Insn::NeqF(out, a, b),
                    reifier::BinOp::Lt if a_ty.is_float() => Insn::LtF(out, a, b),
                    reifier::BinOp::Leq if a_ty.is_float() => Insn::LeqF(out, a, b),
                    reifier::BinOp::Gt if a_ty.is_float() => Insn::LtF(out, b, a),
                    reifier::BinOp::Geq if a_ty.is_float() => Insn::LeqF(out, b, a),
                    reifier::BinOp::Eq
                    | reifier::BinOp::Neq
                    | reifier::BinOp::Lt
                    | reifier::BinOp::Leq
                    | reifier::BinOp::Gt
                    | reifier::BinOp::Geq => panic!("Unexpected type for comparison"),
                    reifier::BinOp::Recv => todo!(),
                    reifier::BinOp::BitOr => Insn::BitOrI(out, a, b),
                    reifier::BinOp::BitXor => Insn::BitXorI(out, a, b),
                    reifier::BinOp::BitAnd => Insn::BitAndI(out, a, b),
                    reifier::BinOp::Shl => Insn::BitShlI(out, a, b),
                    reifier::BinOp::Shr => Insn::BitShrI(out, a, b),
                    reifier::BinOp::Mod => Insn::ModI(out, a, b),
                };

                self.insn(insn);

                Some(out)
            }
            reifier::ExprKind::Unary(reifier::UnOp::Neg, a) if want_output => {
                let a = self.expr(a, true).unwrap();
                let out = self.new_temp(Kind::Integer);
                self.insn(Insn::NegI(out, a));
                Some(out)
            }
            reifier::ExprKind::Unary(reifier::UnOp::Not, a) if want_output => {
                let a = self.expr(a, true).unwrap();
                let out = self.new_temp(Kind::Integer);
                self.insn(Insn::BoolNotI(out, a));
                Some(out)
            }
            reifier::ExprKind::Unary(_, a) => self.expr(a, false),
            reifier::ExprKind::Call(base, arg) => {
                let base = self.expr(base, true).unwrap();
                let args = if let reifier::Type::Tuple(items) = &arg.ty {
                    if items.len() > 0 {
                        let arg = self.expr(arg, true).unwrap();
                        let mut args = Vec::with_capacity(items.len());
                        for i in 0..items.len() {
                            let temp = self.new_temp(Kind::of(&items[i]));
                            self.insn(Insn::Load(temp, arg, i as u64 * 8));
                            args.push(temp);
                        }

                        args.into_boxed_slice()
                    } else {
                        Box::new([])
                    }
                } else {
                    Box::new([self.expr(arg, true).unwrap()])
                };

                let out = self.new_temp(Kind::of(&arg.ty));
                self.insn(Insn::Call(out, base, args));

                if want_output {
                    Some(out)
                } else {
                    None
                }
            }
            reifier::ExprKind::Construct(_sym, data) => self.expr(data, want_output),
            reifier::ExprKind::Variant(name, data) => {
                if want_output {
                    let item_ty = VariantItemType {
                        name: *name,
                        inner: data.as_ref().map(|d| d.ty.clone()),
                    };

                    let id = self.variant_id(item_ty);

                    let out = self.new_temp(Kind::Integer);
                    self.alloc(out, if data.is_some() { 16 } else { 8 });
                    let id_temp = self.new_temp(Kind::Integer);
                    self.insn(Insn::ConstI(id_temp, id));
                    self.insn(Insn::Store(out, id_temp, 0));
                    if let Some(data) = data {
                        let data = self.expr(&data, true).unwrap();
                        self.insn(Insn::Store(out, data, 8));
                    }

                    Some(out)
                } else {
                    if let Some(data) = data {
                        self.expr(data, false)
                    } else {
                        None
                    }
                }
            }
            reifier::ExprKind::Symbol(_) if !want_output => None,
            reifier::ExprKind::Symbol(sym) => Some(self.symbol(sym)),
            reifier::ExprKind::Literal(_) if !want_output => None,
            reifier::ExprKind::Literal(lit) => {
                let out = self.new_temp(Kind::of(&expr.ty));
                match lit {
                    &reifier::Literal::Boolean(b) => self.insn(Insn::ConstI(out, b as u64)),
                    &reifier::Literal::Integer(i) => self.insn(Insn::ConstI(out, i)),
                    &reifier::Literal::Float(f) => self.insn(Insn::ConstF(out, f)),
                    _ => (),
                }

                Some(out)
            }
        }
    }

    fn symbol(&mut self, sym: &reifier::Symbol) -> TempRef {
        if let Some(local_temp) = self.locals.get(sym) {
            *local_temp
        } else if let Some(def) = self.reified_module.defs.get(sym) {
            let temp = self.new_temp(Kind::of(&def.ty));
            self.insn(Insn::LoadSym(temp, *sym));
            temp
        } else if let Some(builtin) = self.reified_module.builtin_funcs.get(sym) {
            let temp = self.new_temp(Kind::Integer);
            self.insn(Insn::LoadBuiltin(temp, builtin.0));
            temp
        } else {
            panic!("symbol not found: {sym:?}")
        }
    }

    fn variant_id(&mut self, item_ty: VariantItemType<'a>) -> u64 {
        if let Some(&id) = self.variant_ids.get(&item_ty) {
            id
        } else {
            let id = self.variant_id_counter;
            self.variant_id_counter += 1;
            self.variant_ids.insert(item_ty.clone(), id);
            id
        }
    }

    fn cond(&mut self, expr: &reifier::Expr<'a>, jump_cond: JumpCond) {
        match &expr.kind {
            reifier::ExprKind::Scope(scope) => {
                let len = scope.exprs.len();
                if scope.discard || len == 0 {
                    panic!("invalid scope in condition? {expr:?}");
                }

                for (i, expr) in scope.exprs.iter().enumerate() {
                    if i == len - 1 {
                        self.cond(expr, jump_cond);
                    } else {
                        self.expr(expr, false);
                    }
                }
            }
            reifier::ExprKind::Case {
                cond,
                on_true,
                on_false,
            } => {
                let on_false_lab = self.new_label();
                let done_lab = self.new_label();
                self.cond(cond, JumpCond::False(on_false_lab));
                self.cond(on_true, jump_cond);
                self.ctrl(Ctrl::Jump(done_lab));
                self.set_label_target(on_false_lab);
                if let Some(on_false) = on_false {
                    self.cond(on_false, jump_cond);
                }
                self.set_label_target(done_lab);
            }
            reifier::ExprKind::StructuralEq(pat, val) => {
                let mut assignments = Vec::new();
                let val = self.expr(val, true).unwrap();
                match jump_cond {
                    JumpCond::False(on_false) => {
                        self.structural_eq(pat, val, on_false, &mut assignments);
                        for (sym, temp) in assignments {
                            self.insn(Insn::Copy(*self.locals.get(&sym).unwrap(), temp));
                        }
                    }
                    JumpCond::True(on_true) => {
                        let on_false = self.new_label();
                        self.structural_eq(pat, val, on_false, &mut assignments);
                        for (sym, temp) in assignments {
                            self.insn(Insn::Copy(*self.locals.get(&sym).unwrap(), temp));
                        }
                        self.ctrl(Ctrl::Jump(on_true));
                        self.set_label_target(on_false);
                    }
                };
            }
            reifier::ExprKind::Binary(reifier::BinOp::And, a, b) => match jump_cond {
                JumpCond::False(on_false) => {
                    self.cond(a, JumpCond::False(on_false));
                    self.cond(b, JumpCond::False(on_false));
                }
                JumpCond::True(on_true) => {
                    let on_false = self.new_label();
                    self.cond(a, JumpCond::False(on_false));
                    self.cond(b, JumpCond::True(on_true));
                    self.set_label_target(on_false);
                }
            },
            reifier::ExprKind::Binary(reifier::BinOp::Or, a, b) => match jump_cond {
                JumpCond::True(on_true) => {
                    self.cond(a, JumpCond::True(on_true));
                    self.cond(b, JumpCond::True(on_true));
                }
                JumpCond::False(on_false) => {
                    let on_true = self.new_label();
                    self.cond(a, JumpCond::True(on_true));
                    self.cond(b, JumpCond::False(on_false));
                    self.set_label_target(on_true);
                }
            },
            reifier::ExprKind::Binary(
                op @ (reifier::BinOp::Eq
                | reifier::BinOp::Neq
                | reifier::BinOp::Lt
                | reifier::BinOp::Leq
                | reifier::BinOp::Gt
                | reifier::BinOp::Geq),
                a,
                b,
            ) => {
                let a = self.expr(a, true).unwrap();
                let b = self.expr(b, true).unwrap();

                let (cmp, a, b, lab) = match jump_cond {
                    JumpCond::False(on_false) => match op {
                        reifier::BinOp::Eq => (BranchCmp::Neq, a, b, on_false),
                        reifier::BinOp::Neq => (BranchCmp::Eq, a, b, on_false),
                        reifier::BinOp::Lt => (BranchCmp::Geq, a, b, on_false),
                        reifier::BinOp::Leq => (BranchCmp::Lt, b, a, on_false),
                        reifier::BinOp::Gt => (BranchCmp::Geq, b, a, on_false),
                        reifier::BinOp::Geq => (BranchCmp::Lt, a, b, on_false),
                        _ => unreachable!(),
                    },
                    JumpCond::True(on_true) => match op {
                        reifier::BinOp::Eq => (BranchCmp::Eq, a, b, on_true),
                        reifier::BinOp::Neq => (BranchCmp::Neq, a, b, on_true),
                        reifier::BinOp::Lt => (BranchCmp::Lt, a, b, on_true),
                        reifier::BinOp::Leq => (BranchCmp::Geq, b, a, on_true),
                        reifier::BinOp::Gt => (BranchCmp::Lt, b, a, on_true),
                        reifier::BinOp::Geq => (BranchCmp::Geq, a, b, on_true),
                        _ => unreachable!(),
                    },
                };

                self.branch(Branch(cmp, a, b, lab))
            }
            reifier::ExprKind::Unary(reifier::UnOp::Not, e) => {
                self.cond(
                    &e,
                    match jump_cond {
                        JumpCond::True(on_true) => JumpCond::False(on_true),
                        JumpCond::False(on_false) => JumpCond::True(on_false),
                    },
                );
            }
            reifier::ExprKind::Call(..) => {
                let res = self.expr(expr, true).unwrap();
                let zero = self.new_temp(Kind::Integer);
                self.insn(Insn::ConstI(zero, 0));
                let (cmp, lab) = match jump_cond {
                    JumpCond::True(lab) => (BranchCmp::Neq, lab),
                    JumpCond::False(lab) => (BranchCmp::Eq, lab),
                };
                self.branch(Branch(cmp, res, zero, lab));
            }
            reifier::ExprKind::Symbol(sym) => {
                let zero = self.new_temp(Kind::Integer);
                self.insn(Insn::ConstI(zero, 0));
                let temp = {
                    if let Some(local_temp) = self.locals.get(sym) {
                        *local_temp
                    } else if let Some(def) = self.reified_module.defs.get(sym) {
                        let temp = self.new_temp(Kind::of(&def.body.ty));
                        self.insn(Insn::LoadSym(temp, *sym));
                        temp
                    } else if let Some(builtin) = self.reified_module.builtin_funcs.get(sym) {
                        let temp = self.new_temp(Kind::Integer);
                        self.insn(Insn::LoadSym(temp, *sym));
                        temp
                    } else {
                        panic!("symbol not found: {expr:?}")
                    }
                };
                let (cmp, lab) = match jump_cond {
                    JumpCond::True(lab) => (BranchCmp::Neq, lab),
                    JumpCond::False(lab) => (BranchCmp::Eq, lab),
                };
                self.branch(Branch(cmp, temp, zero, lab));
            }
            reifier::ExprKind::Literal(reifier::Literal::Boolean(b)) => {
                if *b {
                    if let JumpCond::True(on_true) = jump_cond {
                        self.ctrl(Ctrl::Jump(on_true));
                    }
                } else {
                    if let JumpCond::False(on_false) = jump_cond {
                        self.ctrl(Ctrl::Jump(on_false))
                    }
                }
            }
            reifier::ExprKind::Literal(..)
            | reifier::ExprKind::Construct(..)
            | reifier::ExprKind::Variant(..)
            | reifier::ExprKind::Unary(reifier::UnOp::Neg, ..)
            | reifier::ExprKind::Binary(..)
            | reifier::ExprKind::Tuple(..)
            | reifier::ExprKind::For { .. }
            | reifier::ExprKind::Abstract { .. } => panic!("invalid type for cond? {expr:?}"),
        }
    }

    fn structural_eq(
        &mut self,
        pat: &reifier::Pattern<'a>,
        val: TempRef,
        fail_lab: LabelRef,
        assignments: &mut Vec<(reifier::Symbol, TempRef)>,
    ) {
        match &pat.kind {
            reifier::PatternKind::Apply(_a, b) => self.structural_eq(b, val, fail_lab, assignments),
            reifier::PatternKind::Variant(name, data) => {
                let item_ty = VariantItemType {
                    name: *name,
                    inner: data.as_ref().map(|d| d.ty.clone()),
                };

                let pat_id = self.variant_id(item_ty);
                let pat_id_temp = self.new_temp(Kind::Integer);
                self.insn(Insn::ConstI(pat_id_temp, pat_id));
                let val_id_temp = self.new_temp(Kind::Integer);
                self.insn(Insn::Load(val_id_temp, val, 0));
                self.branch(Branch(BranchCmp::Neq, pat_id_temp, val_id_temp, fail_lab));
                if let Some(data) = &data {
                    let val_data = self.new_temp(Kind::of(&data.ty));
                    self.insn(Insn::Load(val_data, val, 8));
                    self.structural_eq(data, val_data, fail_lab, assignments);
                }
            }
            reifier::PatternKind::Tuple(items) => {
                for (i, item) in items.iter().enumerate() {
                    let item_val = self.new_temp(Kind::of(&item.ty));
                    self.insn(Insn::Load(item_val, val, i as u64 * 8));
                    self.structural_eq(item, item_val, fail_lab, assignments);
                }
            }
            &reifier::PatternKind::Solve(
                reifier::SolveMarker::Val | reifier::SolveMarker::Var,
                sym,
            ) => {
                self.locals.insert(sym, val);
            }
            &reifier::PatternKind::Solve(reifier::SolveMarker::Set, sym) => {
                assignments.push((sym, val));
            }
            reifier::PatternKind::Symbol(_) => panic!("unexpected symbol in pattern"),
        }
    }

    fn abstract_arg(&mut self, pat: &reifier::Pattern<'a>, val: TempRef) {
        match &pat.kind {
            reifier::PatternKind::Apply(_a, b) => self.abstract_arg(b, val),
            reifier::PatternKind::Tuple(items) => {
                for (i, item) in items.iter().enumerate() {
                    let item_val = self.new_temp(Kind::of(&item.ty));
                    self.insn(Insn::Load(item_val, val, i as u64 * 8));
                    self.abstract_arg(item, item_val);
                }
            }
            &reifier::PatternKind::Solve(
                reifier::SolveMarker::Val | reifier::SolveMarker::Var,
                sym,
            ) => {
                self.locals.insert(sym, val);
            }
            reifier::PatternKind::Symbol(_)
            | reifier::PatternKind::Solve(reifier::SolveMarker::Set, ..)
            | reifier::PatternKind::Variant(..) => {
                panic!("unsupported in abstraction arg: {pat:?}")
            }
        }
    }

    fn alloc(&mut self, temp: TempRef, bytes: u64) {
        let alloc_temp = self.new_temp(Kind::Integer);
        let bytes_temp = self.new_temp(Kind::Integer);
        self.insn(Insn::LoadBuiltin(alloc_temp, reifier::Builtin::Alloc));
        self.insn(Insn::ConstI(bytes_temp, bytes));
        self.insn(Insn::Call(temp, alloc_temp, Box::new([bytes_temp])));
    }

    fn new_temp(&mut self, kind: Kind) -> TempRef {
        let idx = self.temps.len();
        self.temps.push(TempInfo { kind });
        TempRef(idx)
    }

    fn new_label(&mut self) -> LabelRef {
        let idx = self.labels.len();
        self.labels.push(None);
        LabelRef(idx)
    }

    fn set_label_target(&mut self, label: LabelRef) {
        self.finalize_block();
        self.labels[label.0] = Some(BlockRef(self.blocks.len()));
    }

    fn insn(&mut self, insn: Insn) {
        self.current_insns.push(insn);
    }

    fn ctrl(&mut self, ctrl: Ctrl<LabelRef>) {
        assert!(matches!(self.current_ctrl, None));

        self.current_ctrl = Some(ctrl);
        self.finalize_block();
    }

    fn branch(&mut self, branch: Branch<LabelRef>) {
        if self.current_branch.is_some() {
            self.finalize_block();
        }

        self.current_branch = Some(branch);
    }

    fn finalize_block(&mut self) {
        let current_insns = std::mem::take(&mut self.current_insns);
        let current_branch = self.current_branch.take();
        let current_ctrl = if let Some(current_ctrl) = self.current_ctrl.take() {
            current_ctrl
        } else {
            let next_label = self.new_label();
            self.labels[next_label.0] = Some(BlockRef(self.blocks.len() + 1));
            Ctrl::Jump(next_label)
        };

        self.blocks
            .push((current_insns, current_branch, current_ctrl));
    }
}

#[derive(Debug, Clone, Copy)]
enum JumpCond {
    True(LabelRef),
    False(LabelRef),
}
