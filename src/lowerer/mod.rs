use std::ops::Deref;

use crate::reifier::{self, VariantItemType};

mod cfg;
pub use cfg::*;
use rustc_hash::FxHashMap;

pub fn cfg<'s>(module: reifier::Module<'s>) -> Module<'s> {
    todo!()
}

fn tac<'s>(module: reifier::Module<'s>) -> Tac {
    let mut builder = TacBuilder {
        module: &module,
        local_to_temp: FxHashMap::default(),
        variant_ids: FxHashMap::default(),
        variant_id_counter: 0,
        labels: FxHashMap::default(),
        label_counter: 0,
        insns: Vec::new(),
    };

    todo!()
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct TacLabel(usize);

struct TacBuilder<'a> {
    module: &'a reifier::Module<'a>,
    local_to_temp: FxHashMap<reifier::Symbol, TempId>,
    variant_ids: FxHashMap<reifier::VariantItemType<'a>, u64>,
    variant_id_counter: u64,
    labels: FxHashMap<TacLabel, usize>,
    label_counter: usize,
    insns: Vec<AnyInsn<TacLabel>>,
}

impl<'a> TacBuilder<'a> {
    fn tac(
        mut self,
        spec: bool,
        arg: Option<&reifier::Pattern>,
        body: &reifier::Expr<'a>,
    ) -> Option<TempId> {
        //self.expr(body)
        todo!()
    }

    fn scope(&mut self, scope: &reifier::Scope<'a>, want_output: bool) -> Option<TempId> {
        let len = scope.exprs.len();
        for (i, expr) in scope.exprs.iter().enumerate() {
            if i == len - 1 && !scope.discard {
                return self.expr(expr, want_output);
            } else {
                self.expr(expr, false);
            }
        }

        None
    }

    fn expr(&mut self, expr: &reifier::Expr<'a>, want_output: bool) -> Option<TempId> {
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

                None
            }
            reifier::ExprKind::Abstract { spec, arg, body } => todo!(),
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
                self.gen_ctrl(CtrlInsn::Jump(cond_lab));
                self.set_label_target(body_lab);
                self.expr(body, false);
                if let Some(afterthought) = afterthought {
                    self.expr(afterthought, false);
                }
                self.cond(&cond, JumpCond::True(body_lab));

                if want_output {
                    let empty = self.new_temp();
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
                let res = self.new_temp();
                self.cond(cond, JumpCond::False(on_false_lab));
                let true_res = self.expr(on_true, want_output && on_false.is_some()).unwrap();
                if want_output && on_false.is_some() {
                    self.gen_insn(Insn::Copy(res, true_res));
                }
                self.gen_ctrl(CtrlInsn::Jump(done_lab));
                self.set_label_target(on_false_lab);
                if let Some(on_false) = on_false {
                    let false_res = self.expr(on_false, want_output).unwrap();
                    self.gen_insn(Insn::Copy(res, false_res));
                    self.set_label_target(done_lab);
                } else {
                    self.set_label_target(done_lab);
                }

                if want_output {
                    if on_false.is_some() {
                        Some(res)
                    } else {
                        let empty = self.new_temp();
                        self.alloc(empty, 0);
                        Some(empty)
                    }
                } else {
                    None
                }
            }
            reifier::ExprKind::Tuple(items) => {
                let out = self.new_temp();
                self.alloc(out, items.len() * 8);
                for (i, item) in items.iter().enumerate() {
                    let item_temp = self.expr(item, true).unwrap();
                    self.gen_insn(Insn::Store(out, item_temp, i as u64 * 8));
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
                let out = self.new_temp();
                let mut assignments = Vec::new();
                self.structural_eq(pat, val, fail_lab, &mut assignments);
                for (sym, temp) in assignments {
                    self.gen_insn(Insn::Copy(*self.local_to_temp.get(&sym).unwrap(), temp));
                }
                if want_output {
                    self.gen_insn(Insn::ConstI(out, 1));
                    self.gen_ctrl(CtrlInsn::Jump(end_lab));
                }
                self.set_label_target(fail_lab);
                if want_output {
                    self.gen_insn(Insn::ConstI(out, 0));
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
                let out = self.new_temp();
                let zero = self.new_temp();
                self.gen_insn(Insn::ConstI(zero, 0));
                let a = self.expr(a, true).unwrap();
                self.gen_ctrl(CtrlInsn::Branch(BranchCmp::Neq, a, zero, lab));
                self.gen_insn(Insn::Copy(out, a));
                self.gen_ctrl(CtrlInsn::Jump(endlab));
                self.set_label_target(lab);
                let b = self.expr(b, true).unwrap();
                self.gen_insn(Insn::Copy(out, b));
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
                let out = self.new_temp();
                let zero = self.new_temp();
                self.gen_insn(Insn::ConstI(zero, 0));
                let a = self.expr(a, true).unwrap();
                self.gen_ctrl(CtrlInsn::Branch(BranchCmp::Eq, a, zero, lab));
                self.gen_insn(Insn::Copy(out, a));
                self.gen_ctrl(CtrlInsn::Jump(endlab));
                self.set_label_target(lab);
                let b = self.expr(b, true).unwrap();
                self.gen_insn(Insn::Copy(out, b));
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
                let a = self.expr(a, true).unwrap();
                let b = self.expr(b, true).unwrap();
                let out = self.new_temp();

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
                    reifier::BinOp::Eq if expr.ty.is_int() => Insn::EqI(out, a, b),
                    reifier::BinOp::Neq if expr.ty.is_int() => Insn::NeqI(out, a, b),
                    reifier::BinOp::Lt if expr.ty.is_int() => Insn::LtI(out, a, b),
                    reifier::BinOp::Leq if expr.ty.is_int() => Insn::LeqI(out, a, b),
                    reifier::BinOp::Gt if expr.ty.is_int() => Insn::LtI(out, b, a),
                    reifier::BinOp::Geq if expr.ty.is_int() => Insn::LeqI(out, b, a),
                    reifier::BinOp::Eq if expr.ty.is_float() => Insn::EqF(out, a, b),
                    reifier::BinOp::Neq if expr.ty.is_float() => Insn::NeqF(out, a, b),
                    reifier::BinOp::Lt if expr.ty.is_float() => Insn::LtF(out, a, b),
                    reifier::BinOp::Leq if expr.ty.is_float() => Insn::LeqF(out, a, b),
                    reifier::BinOp::Gt if expr.ty.is_float() => Insn::LtF(out, b, a),
                    reifier::BinOp::Geq if expr.ty.is_float() => Insn::LeqF(out, b, a),
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

                self.gen_insn(insn);

                Some(out)
            }
            reifier::ExprKind::Unary(reifier::UnOp::Neg, a) if want_output => {
                let a = self.expr(a, true).unwrap();
                let out = self.new_temp();
                self.gen_insn(Insn::NegI(out, a));
                Some(out)
            }
            reifier::ExprKind::Unary(reifier::UnOp::Not, a) if want_output => {
                let a = self.expr(a, true).unwrap();
                let out = self.new_temp();
                self.gen_insn(Insn::BoolNotI(out, a));
                Some(out)
            }
            reifier::ExprKind::Unary(_, a) => self.expr(a, false),
            reifier::ExprKind::Call(base, arg) => {
                let out = self.new_temp();
                let base = self.expr(base, true).unwrap();
                let args = if let reifier::Type::Tuple(items) = &arg.ty {
                    let arg = self.expr(expr, true).unwrap();
                    let mut args = Vec::with_capacity(items.len());
                    for i in 0..items.len() {
                        let temp = self.new_temp();
                        self.gen_insn(Insn::Load(temp, arg, i as u64 * 8));
                        args.push(temp);
                    }

                    args.into_boxed_slice()
                } else {
                    Box::new([self.expr(expr, true).unwrap()])
                };

                self.gen_ctrl(CtrlInsn::Call(out, base, args));

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

                    let out = self.new_temp();
                    self.alloc(out, if data.is_some() { 16 } else { 8 });
                    let id_temp = self.new_temp();
                    self.gen_insn(Insn::ConstI(id_temp, id));
                    self.gen_insn(Insn::Store(out, id_temp, 0));
                    if let Some(data) = data {
                        let data = self.expr(&data, true).unwrap();
                        self.gen_insn(Insn::Store(out, data, 8));
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
            reifier::ExprKind::Symbol(sym) => Some(*self.local_to_temp.get(sym).unwrap()),
            reifier::ExprKind::Literal(_) if !want_output => None,
            reifier::ExprKind::Literal(lit) => {
                let out = self.new_temp();
                match lit {
                    &reifier::Literal::Boolean(b) => self.gen_insn(Insn::ConstI(out, b as u64)),
                    &reifier::Literal::Integer(i) => self.gen_insn(Insn::ConstI(out, i)),
                    &reifier::Literal::Float(f) => self.gen_insn(Insn::ConstF(out, f)),
                    _ => (),
                }

                Some(out)
            }
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
            reifier::ExprKind::Case { .. } => {
                self.expr(expr, true).unwrap();
                todo!()
            }
            reifier::ExprKind::StructuralEq(pat, val) => {
                let mut assignments = Vec::new();
                let val = self.expr(val, true).unwrap();
                match jump_cond {
                    JumpCond::False(on_false) => {
                        self.structural_eq(pat, val, on_false, &mut assignments);
                        for (sym, temp) in assignments {
                            self.gen_insn(Insn::Copy(*self.local_to_temp.get(&sym).unwrap(), temp));
                        }
                    }
                    JumpCond::True(on_true) => {
                        let on_false = self.new_label();
                        self.structural_eq(pat, val, on_false, &mut assignments);
                        for (sym, temp) in assignments {
                            self.gen_insn(Insn::Copy(*self.local_to_temp.get(&sym).unwrap(), temp));
                        }
                        self.gen_ctrl(CtrlInsn::Jump(on_true));
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

                self.gen_ctrl(CtrlInsn::Branch(cmp, a, b, lab))
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
                let zero = self.new_temp();
                self.gen_insn(Insn::ConstI(zero, 0));
                let (cmp, lab) = match jump_cond {
                    JumpCond::True(lab) => (BranchCmp::Neq, lab),
                    JumpCond::False(lab) => (BranchCmp::Eq, lab),
                };
                self.gen_ctrl(CtrlInsn::Branch(cmp, res, zero, lab));
            }
            reifier::ExprKind::Symbol(sym) => {
                let zero = self.new_temp();
                self.gen_insn(Insn::ConstI(zero, 0));
                let temp = *self.local_to_temp.get(sym).unwrap();
                let (cmp, lab) = match jump_cond {
                    JumpCond::True(lab) => (BranchCmp::Neq, lab),
                    JumpCond::False(lab) => (BranchCmp::Eq, lab),
                };
                self.gen_ctrl(CtrlInsn::Branch(cmp, temp, zero, lab));
            }
            reifier::ExprKind::Literal(reifier::Literal::Boolean(b)) => {
                if *b {
                    if let JumpCond::True(on_true) = jump_cond {
                        self.gen_ctrl(CtrlInsn::Jump(on_true));
                    }
                } else {
                    if let JumpCond::False(on_false) = jump_cond {
                        self.gen_ctrl(CtrlInsn::Jump(on_false))
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
        val: TempId,
        fail_lab: TacLabel,
        assignments: &mut Vec<(reifier::Symbol, TempId)>,
    ) {
        match &pat.kind {
            reifier::PatternKind::Apply(_a, b) => self.structural_eq(b, val, fail_lab, assignments),
            reifier::PatternKind::Variant(name, data) => {
                let item_ty = VariantItemType {
                    name: *name,
                    inner: data.as_ref().map(|d| d.ty.clone()),
                };

                let pat_id = self.variant_id(item_ty);
                let pat_id_temp = self.new_temp();
                self.gen_insn(Insn::ConstI(pat_id_temp, pat_id));
                let val_id_temp = self.new_temp();
                self.gen_insn(Insn::Load(val_id_temp, val, 0));
                self.gen_ctrl(CtrlInsn::Branch(
                    BranchCmp::Neq,
                    pat_id_temp,
                    val_id_temp,
                    fail_lab,
                ));
                if let Some(data) = &data {
                    let val_data = self.new_temp();
                    self.gen_insn(Insn::Load(val_data, val, 8));
                    self.structural_eq(data, val_data, fail_lab, assignments);
                }
            }
            reifier::PatternKind::Tuple(items) => {
                for (i, item) in items.iter().enumerate() {
                    let item_val = self.new_temp();
                    self.gen_insn(Insn::Load(item_val, val, i as u64 * 8));
                    self.structural_eq(item, item_val, fail_lab, assignments);
                }
            }
            &reifier::PatternKind::Solve(
                reifier::SolveMarker::Val | reifier::SolveMarker::Var,
                sym,
            ) => {
                self.local_to_temp.insert(sym, val);
            }
            &reifier::PatternKind::Solve(reifier::SolveMarker::Set, sym) => {
                assignments.push((sym, val));
            }
            reifier::PatternKind::Symbol(_) => panic!("unexpected symbol in pattern"),
        }
    }

    fn new_temp(&mut self) -> TempId {
        todo!()
    }

    fn new_label(&mut self) -> TacLabel {
        let id = self.label_counter;
        self.label_counter += 1;
        TacLabel(id)
    }

    fn set_label_target(&mut self, label: TacLabel) {
        self.labels.insert(label, self.insns.len());
    }

    fn alloc(&mut self, temp: TempId, bytes: usize) {
        todo!()
    }

    fn gen_insn(&mut self, insn: Insn) {
        self.insns.push(AnyInsn::Insn(insn));
    }

    fn gen_ctrl(&mut self, ctrl: CtrlInsn<TacLabel>) {
        self.insns.push(AnyInsn::Ctrl(ctrl));
    }
}

#[derive(Debug, Clone, Copy)]
enum JumpCond {
    True(TacLabel),
    False(TacLabel),
}
