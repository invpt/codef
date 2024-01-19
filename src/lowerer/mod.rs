use crate::reifier::{self, Builtin, VariantItemType};

use rustc_hash::FxHashMap;

mod lir;
pub use lir::*;

pub fn lower<'s>(module: &reifier::Module<'s>) -> Module<'s> {
    let mut defs = FxHashMap::default();

    for (sym, def) in &module.defs {
        let reifier::ExprKind::Abstract { spec, arg, body } = &def.body.kind else {
            continue;
        };
        defs.insert(
            *sym,
            Def {
                name: def.name,
                value: Value::Function(Lowerer::new(module).lower(*spec, arg.as_ref(), &body)),
            },
        );
    }

    Module { main: None, defs }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct LabelRef(usize);

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
struct VarRef(usize);

#[derive(Debug)]
struct Lowerer<'a> {
    // the module we're lowering
    reified_module: &'a reifier::Module<'a>,

    // counter to manage all the temp handles we're giving out
    temp_counter: usize,

    // data so we can assign a numeric id to each variant
    variant_ids: FxHashMap<reifier::VariantItemType<'a>, u64>,
    variant_id_counter: u64,

    // lookup tables for locals
    vals: FxHashMap<reifier::Symbol, Temp>,
    vars: FxHashMap<reifier::Symbol, VarRef>,

    // generations of variables
    generations: Vec<Temp>,

    // arguments
    args: Vec<Temp>,

    // the cfg, as built so far
    labels: Vec<Option<BlockRef>>,
    blocks: Vec<Block<LabelRef>>,
    block_meta: Vec<BlockMeta>,

    // the block we're currently working on
    insns: Vec<Insn>,
    branch: Option<Branch<LabelRef>>,
    live: FxHashMap<VarRef, Temp>,
}

#[derive(Debug)]
struct BlockMeta {
    param_vars: Vec<VarRef>,
    generations: Vec<Temp>,
}

impl<'a> Lowerer<'a> {
    fn new(reified_module: &'a reifier::Module<'a>) -> Lowerer<'a> {
        Lowerer {
            args: Vec::new(),
            labels: Vec::new(),
            blocks: Vec::new(),
            block_meta: Vec::new(),
            insns: Vec::new(),
            branch: None,
            live: FxHashMap::default(),
            vals: FxHashMap::default(),
            vars: FxHashMap::default(),
            generations: Vec::new(),
            variant_ids: FxHashMap::default(),
            variant_id_counter: 0,
            temp_counter: 0,
            reified_module,
        }
    }

    fn lower(
        mut self,
        spec: bool,
        param: Option<&reifier::Pattern<'a>>,
        body: &reifier::Expr<'a>,
    ) -> Cfg {
        if let Some(param) = param {
            if let reifier::PatternKind::Tuple(items) = &param.kind {
                for item in &**items {
                    let temp = self.new_temp(Kind::of(&item.ty));
                    self.args.push(temp);
                    self.abstract_arg(item, temp);
                }
            } else {
                let temp = self.new_temp(Kind::of(&param.ty));
                self.args.push(temp);
                self.abstract_arg(param, temp);
            }
        }

        if !spec {
            let ret = self.expr(body, true).unwrap();
            self.ctrl(Ctrl::Return(ret));
        } else {
            // 1. pack all the args into one tuple
            let reifying_args = self.alloc(8 * self.args.len() as u64);
            for (i, &arg) in self.args.clone().iter().enumerate() {
                self.store(MemRef(reifying_args, i as u64 * 8), arg);
            }

            let reifying_args_pat = param.expect("Cannot reify with empty arguments");

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

            let proc_temp = self.load(Producer::Ir(proc));
            let spec_temp = self.load(Producer::Builtin(Builtin::Spec));
            let spec_res_temp = self.load(Producer::Call(
                spec_temp,
                Box::new([proc_temp, arg_temp]),
                Kind::Integer,
            ));
            self.ctrl(Ctrl::Return(spec_res_temp))
        }

        let mut blocks = Vec::new();
        for (
            i,
            Block {
                insns,
                branch,
                ctrl,
                params,
            },
        ) in std::mem::take(&mut self.blocks).into_iter().enumerate()
        {
            blocks.push(Block {
                params,
                insns,
                branch: if let Some(Branch(cmp, a, b, t)) = branch {
                    Some(Branch(
                        cmp,
                        a,
                        b,
                        self.conv_target(BlockRef(i), self.labels[t.0].unwrap()),
                    ))
                } else {
                    None
                },
                ctrl: match ctrl {
                    Ctrl::Jump(t) => {
                        Ctrl::Jump(self.conv_target(BlockRef(i), self.labels[t.0].unwrap()))
                    }
                    Ctrl::Return(r) => Ctrl::Return(r),
                },
            })
        }

        Cfg {
            temps: self.temp_counter,
            params: self.args.into_boxed_slice(),
            blocks: blocks.into_boxed_slice(),
            entry: BlockRef(0),
        }
    }

    fn conv_target(&self, source: BlockRef, target: BlockRef) -> Target {
        let source_meta = &self.block_meta[source.0];
        let target_meta = &self.block_meta[target.0];

        // we give all of the reqired params by taking the current value of each variable
        let mut args = Vec::with_capacity(target_meta.param_vars.len());
        for var in &*target_meta.param_vars {
            args.push(source_meta.generations[var.0])
        }

        Target {
            block: target,
            arguments: args.into_boxed_slice(),
        }
    }

    fn expr(&mut self, expr: &reifier::Expr<'a>, want_output: bool) -> Option<Temp> {
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
                    Some(self.alloc(0))
                } else {
                    None
                }
            }
            reifier::ExprKind::Abstract { .. } => todo!("general closures"),
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
                    Some(self.alloc(0))
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
                let res;
                self.cond(cond, JumpCond::False(on_false_lab));
                let true_res = self.expr(on_true, want_output && on_false.is_some());
                if let Some(on_false) = on_false {
                    if want_output {
                        res = self.new_var(true_res.unwrap());
                        self.ctrl(Ctrl::Jump(done_lab));
                        self.set_label_target(on_false_lab);
                        let false_res = self.expr(on_false, true).unwrap();
                        self.set_var(res, false_res);
                        self.set_label_target(done_lab);

                        Some(self.get_var(res))
                    } else {
                        self.ctrl(Ctrl::Jump(done_lab));
                        self.set_label_target(on_false_lab);
                        self.expr(on_false, false);
                        self.set_label_target(done_lab);

                        None
                    }
                } else {
                    self.set_label_target(on_false_lab);

                    if want_output {
                        Some(self.alloc(0))
                    } else {
                        None
                    }
                }
            }
            reifier::ExprKind::Tuple(items) => {
                let out = self.alloc(items.len() as u64 * 8);
                for (i, item) in items.iter().enumerate() {
                    let item_temp = self.expr(item, true).unwrap();
                    self.store(MemRef(out, i as u64 * 8), item_temp);
                }

                if want_output {
                    Some(out)
                } else {
                    None
                }
            }
            reifier::ExprKind::StructuralEq(pat, expr) => {
                let val = self.expr(expr, true).unwrap();
                let on_false = self.new_label();
                let end_lab = self.new_label();
                let out;
                let mut assignments = Vec::new();
                self.structural_eq(pat, val, on_false, &mut assignments);
                for (sym, temp) in assignments {
                    let var = self.vars.get(&sym).unwrap();
                    self.set_var(*var, temp);
                }
                if want_output {
                    let one = self.load(Producer::ConstI(1));
                    out = self.new_var(one);
                    self.ctrl(Ctrl::Jump(end_lab));
                    self.set_label_target(on_false);
                    let zero = self.load(Producer::ConstI(0));
                    self.set_var(out, zero);
                    self.set_label_target(end_lab);

                    Some(self.get_var(out))
                } else {
                    self.set_label_target(on_false);

                    None
                }
            }
            reifier::ExprKind::Binary(reifier::BinOp::And, a, b) => {
                let eval_b = self.new_label();
                let end = self.new_label();
                let zero = self.load(Producer::ConstI(0));
                let out;
                {
                    let a = self.expr(a, true).unwrap();
                    self.branch(Branch(BranchCmp::Neq, a, zero, eval_b));
                    out = self.new_var(a);
                    self.ctrl(Ctrl::Jump(end));
                }
                self.set_label_target(eval_b);
                {
                    let b = self.expr(b, true).unwrap();
                    self.set_var(out, b);
                }
                self.set_label_target(end);

                if want_output {
                    Some(self.get_var(out))
                } else {
                    None
                }
            }
            reifier::ExprKind::Binary(reifier::BinOp::Or, a, b) => {
                let eval_b = self.new_label();
                let end = self.new_label();
                let zero = self.load(Producer::ConstI(0));
                let out;
                {
                    let a = self.expr(a, true).unwrap();
                    self.branch(Branch(BranchCmp::Eq, a, zero, eval_b));
                    out = self.new_var(a);
                    self.ctrl(Ctrl::Jump(end));
                }
                self.set_label_target(eval_b);
                {
                    let b = self.expr(b, true).unwrap();
                    self.set_var(out, b);
                }
                self.set_label_target(end);

                if want_output {
                    Some(self.get_var(out))
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

                let (op, flip) = match op {
                    reifier::BinOp::Add if expr.ty.is_int() => (BinOp::AddI, false),
                    reifier::BinOp::Sub if expr.ty.is_int() => (BinOp::SubI, false),
                    reifier::BinOp::Mul if expr.ty.is_int() => (BinOp::MulI, false),
                    reifier::BinOp::Div if expr.ty.is_int() => (BinOp::DivI, false),
                    reifier::BinOp::Add if expr.ty.is_float() => (BinOp::AddF, false),
                    reifier::BinOp::Sub if expr.ty.is_float() => (BinOp::SubF, false),
                    reifier::BinOp::Mul if expr.ty.is_float() => (BinOp::MulF, false),
                    reifier::BinOp::Div if expr.ty.is_float() => (BinOp::DivF, false),
                    reifier::BinOp::Add
                    | reifier::BinOp::Sub
                    | reifier::BinOp::Mul
                    | reifier::BinOp::Div => unreachable!(),
                    reifier::BinOp::Or | reifier::BinOp::And => unreachable!(),
                    reifier::BinOp::Eq if a_ty.is_int() => (BinOp::EqI, false),
                    reifier::BinOp::Neq if a_ty.is_int() => (BinOp::NeqI, false),
                    reifier::BinOp::Lt if a_ty.is_int() => (BinOp::LtI, false),
                    reifier::BinOp::Leq if a_ty.is_int() => (BinOp::LeqI, false),
                    reifier::BinOp::Gt if a_ty.is_int() => (BinOp::LtI, true),
                    reifier::BinOp::Geq if a_ty.is_int() => (BinOp::LeqI, true),
                    reifier::BinOp::Eq if a_ty.is_float() => (BinOp::EqF, false),
                    reifier::BinOp::Neq if a_ty.is_float() => (BinOp::NeqF, false),
                    reifier::BinOp::Lt if a_ty.is_float() => (BinOp::LtF, false),
                    reifier::BinOp::Leq if a_ty.is_float() => (BinOp::LeqF, false),
                    reifier::BinOp::Gt if a_ty.is_float() => (BinOp::LtF, true),
                    reifier::BinOp::Geq if a_ty.is_float() => (BinOp::LeqF, true),
                    reifier::BinOp::Eq
                    | reifier::BinOp::Neq
                    | reifier::BinOp::Lt
                    | reifier::BinOp::Leq
                    | reifier::BinOp::Gt
                    | reifier::BinOp::Geq => unreachable!(),
                    reifier::BinOp::Recv => todo!(),
                    reifier::BinOp::BitOr => (BinOp::BitOrI, false),
                    reifier::BinOp::BitXor => (BinOp::BitXorI, false),
                    reifier::BinOp::BitAnd => (BinOp::BitAndI, false),
                    reifier::BinOp::Shl => (BinOp::BitShlI, false),
                    reifier::BinOp::Shr => (BinOp::BitShrI, false),
                    reifier::BinOp::Mod => (BinOp::ModI, false),
                };

                if flip {
                    Some(self.load(Producer::Binary(op, b, a)))
                } else {
                    Some(self.load(Producer::Binary(op, a, b)))
                }
            }
            reifier::ExprKind::Unary(reifier::UnOp::Neg, a) if want_output => {
                if a.ty.is_int() {
                    let a = self.expr(a, true).unwrap();
                    let out = self.load(Producer::Unary(UnOp::NegI, a));
                    Some(out)
                } else if a.ty.is_float() {
                    let a = self.expr(a, true).unwrap();
                    let out = self.load(Producer::Unary(UnOp::NegF, a));
                    Some(out)
                } else {
                    unreachable!()
                }
            }
            reifier::ExprKind::Unary(reifier::UnOp::Not, a) if want_output => {
                let a = self.expr(a, true).unwrap();
                let out = self.load(Producer::Unary(UnOp::BoolNotI, a));
                Some(out)
            }
            reifier::ExprKind::Unary(_, a) => self.expr(a, false),
            reifier::ExprKind::Apply(base, arg) => {
                if let reifier::ExprKind::Constructor(_) = &base.kind {
                    self.expr(&arg, want_output)
                } else {
                    let base = self.expr(base, true).unwrap();
                    let args = if let reifier::Type::Tuple(items) = &arg.ty {
                        if items.len() > 0 {
                            let arg = self.expr(arg, true).unwrap();
                            let mut args = Vec::with_capacity(items.len());
                            for i in 0..items.len() {
                                let temp = self.load(Producer::Memory(
                                    Kind::of(&items[i]),
                                    MemRef(arg, i as u64 * 8),
                                ));
                                args.push(temp);
                            }

                            args.into_boxed_slice()
                        } else {
                            Box::new([])
                        }
                    } else {
                        Box::new([self.expr(arg, true).unwrap()])
                    };

                    let out = self.load(Producer::Call(base, args, Kind::of(&arg.ty)));

                    if want_output {
                        Some(out)
                    } else {
                        None
                    }
                }
            }
            reifier::ExprKind::Variant(name, data) => {
                if want_output {
                    let item_ty = VariantItemType {
                        name: *name,
                        inner: data.as_ref().map(|d| d.ty.clone()),
                    };

                    let id = self.variant_id(item_ty);

                    let out = self.alloc(if data.is_some() { 16 } else { 8 });
                    let id_temp = self.load(Producer::ConstI(id));
                    self.store(MemRef(out, 0), id_temp);
                    if let Some(data) = data {
                        let data = self.expr(&data, true).unwrap();
                        self.store(MemRef(out, 8), data);
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
            reifier::ExprKind::Load(_) | reifier::ExprKind::Constructor(_) if !want_output => None,
            reifier::ExprKind::Load(sym) | reifier::ExprKind::Constructor(sym) => {
                Some(self.symbol(sym))
            }
            reifier::ExprKind::Literal(_) if !want_output => None,
            reifier::ExprKind::Literal(lit) => Some(match lit {
                &reifier::Literal::Boolean(b) => self.load(Producer::ConstI(b as u64)),
                &reifier::Literal::Integer(i) => self.load(Producer::ConstI(i)),
                &reifier::Literal::Float(f) => self.load(Producer::ConstF(f)),
                _ => todo!(),
            }),
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
                            let var = self.vars.get(&sym).unwrap();
                            self.set_var(*var, temp);
                        }
                    }
                    JumpCond::True(on_true) => {
                        let on_false = self.new_label();
                        self.structural_eq(pat, val, on_false, &mut assignments);
                        for (sym, temp) in assignments {
                            let var = self.vars.get(&sym).unwrap();
                            self.set_var(*var, temp);
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
            reifier::ExprKind::Apply(..) => {
                let res = self.expr(expr, true).unwrap();
                let zero = self.load(Producer::ConstI(0));
                let (cmp, lab) = match jump_cond {
                    JumpCond::True(lab) => (BranchCmp::Neq, lab),
                    JumpCond::False(lab) => (BranchCmp::Eq, lab),
                };
                self.branch(Branch(cmp, res, zero, lab));
            }
            reifier::ExprKind::Load(sym) | reifier::ExprKind::Constructor(sym) => {
                let zero = self.load(Producer::ConstI(0));
                let temp = self.symbol(sym);
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
        val: Temp,
        fail_lab: LabelRef,
        assignments: &mut Vec<(reifier::Symbol, Temp)>,
    ) {
        match &pat.kind {
            reifier::PatternKind::Apply(_a, b) => self.structural_eq(b, val, fail_lab, assignments),
            reifier::PatternKind::Variant(name, data) => {
                let item_ty = VariantItemType {
                    name: *name,
                    inner: data.as_ref().map(|d| d.ty.clone()),
                };

                let pat_id = self.variant_id(item_ty);
                let pat_id_temp = self.load(Producer::ConstI(pat_id));
                let val_id_temp = self.load(Producer::Memory(Kind::Integer, MemRef(val, 0)));
                self.branch(Branch(BranchCmp::Neq, pat_id_temp, val_id_temp, fail_lab));
                if let Some(data) = &data {
                    let val_data = self.load(Producer::Memory(Kind::of(&data.ty), MemRef(val, 8)));
                    self.structural_eq(data, val_data, fail_lab, assignments);
                }
            }
            reifier::PatternKind::Tuple(items) => {
                for (i, item) in items.iter().enumerate() {
                    let item_val = self.load(Producer::Memory(
                        Kind::of(&item.ty),
                        MemRef(val, i as u64 * 8),
                    ));
                    self.structural_eq(item, item_val, fail_lab, assignments);
                }
            }
            &reifier::PatternKind::Solve(reifier::SolveMarker::Val, sym) => {
                self.vals.insert(sym, val);
            }
            &reifier::PatternKind::Solve(reifier::SolveMarker::Var, sym) => {
                let var = self.new_var(val);
                self.vars.insert(sym, var);
            }
            &reifier::PatternKind::Solve(reifier::SolveMarker::Set, sym) => {
                assignments.push((sym, val));
            }
            reifier::PatternKind::Symbol(_) => panic!("unexpected symbol in pattern"),
        }
    }

    fn abstract_arg(&mut self, pat: &reifier::Pattern<'a>, val: Temp) {
        match &pat.kind {
            reifier::PatternKind::Apply(_a, b) => self.abstract_arg(b, val),
            reifier::PatternKind::Tuple(items) => {
                for (i, item) in items.iter().enumerate() {
                    let item_val = self.load(Producer::Memory(
                        Kind::of(&item.ty),
                        MemRef(val, i as u64 * 8),
                    ));
                    self.abstract_arg(item, item_val);
                }
            }
            &reifier::PatternKind::Solve(reifier::SolveMarker::Val, sym) => {
                self.vals.insert(sym, val);
            }
            &reifier::PatternKind::Solve(reifier::SolveMarker::Var, sym) => {
                let var = self.new_var(val);
                self.vars.insert(sym, var);
            }
            reifier::PatternKind::Symbol(_)
            | reifier::PatternKind::Solve(reifier::SolveMarker::Set, ..)
            | reifier::PatternKind::Variant(..) => {
                panic!("unsupported in abstraction arg: {pat:?}")
            }
        }
    }

    fn symbol(&mut self, sym: &reifier::Symbol) -> Temp {
        if let Some(local_temp) = self.vals.get(sym) {
            *local_temp
        } else if let Some(var) = self.vars.get(sym) {
            self.get_var(*var)
        } else if let Some(def) = self.reified_module.defs.get(sym) {
            self.load(Producer::Symbol(Kind::of(&def.body.ty), *sym))
        } else if let Some(builtin) = self.reified_module.builtins.get(sym) {
            self.load(Producer::Builtin(builtin.0))
        } else {
            panic!("symbol not found: {sym:?}")
        }
    }

    fn alloc(&mut self, bytes: u64) -> Temp {
        let alloc = self.load(Producer::Builtin(Builtin::Alloc));
        let bytes = self.load(Producer::ConstI(bytes));
        self.load(Producer::Call(alloc, Box::new([bytes]), Kind::Integer))
    }

    fn new_var(&mut self, temp: Temp) -> VarRef {
        let r = VarRef(self.generations.len());
        self.generations.push(temp);
        r
    }

    fn set_var(&mut self, var: VarRef, temp: Temp) {
        self.generations[var.0] = temp
    }

    fn get_var(&mut self, var: VarRef) -> Temp {
        if self.live.contains_key(&var) {
            self.generations[var.0]
        } else {
            let kind = self.generations[var.0].kind;
            let temp = self.new_temp(kind);
            self.live.insert(var, temp);
            self.generations[var.0] = temp;
            self.generations[var.0]
        }
    }

    fn new_temp(&mut self, kind: Kind) -> Temp {
        let idx = self.temp_counter;
        self.temp_counter += 1;
        Temp { idx, kind }
    }

    fn new_label(&mut self) -> LabelRef {
        let idx = self.labels.len();
        self.labels.push(None);
        LabelRef(idx)
    }

    fn set_label_target(&mut self, label: LabelRef) {
        self.finalize_block(Ctrl::Jump(label));
        self.labels[label.0] = Some(BlockRef(self.blocks.len()));
    }

    fn store(&mut self, dest: MemRef, source: Temp) {
        self.insn(Insn::Store(dest, source));
    }

    fn load(&mut self, producer: Producer) -> Temp {
        let result = self.new_temp(producer.result_kind());
        self.insn(Insn::Load(result, producer));
        result
    }

    fn insn(&mut self, insn: Insn) {
        let lab = self.new_label();
        self.set_label_target(lab);

        self.insns.push(insn);
    }

    fn ctrl(&mut self, ctrl: Ctrl<LabelRef>) {
        self.finalize_block(ctrl);
    }

    fn branch(&mut self, branch: Branch<LabelRef>) {
        let lab = self.new_label();
        self.set_label_target(lab);

        self.branch = Some(branch);
    }

    fn finalize_block(&mut self, ctrl: Ctrl<LabelRef>) {
        let current_insns = std::mem::take(&mut self.insns);
        let current_branch = self.branch.take();
        let current_live = std::mem::take(&mut self.live);
        let mut param_vars = Vec::with_capacity(current_live.len());
        let mut params = Vec::with_capacity(current_live.len());
        for (var, temp) in current_live {
            param_vars.push(var);
            params.push(temp);
        }
        let generations = self.generations.clone();

        self.blocks.push(Block {
            insns: current_insns.into_boxed_slice(),
            branch: current_branch,
            ctrl,
            params: params.into_boxed_slice(),
        });
        self.block_meta.push(BlockMeta {
            param_vars,
            generations,
        });
    }
}

#[derive(Debug, Clone, Copy)]
enum JumpCond {
    True(LabelRef),
    False(LabelRef),
}
