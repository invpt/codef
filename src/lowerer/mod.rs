use crate::reifier;

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
        insns: Vec::new(),
    };

    todo!()
}

struct TacBuilder<'a> {
    module: &'a reifier::Module<'a>,
    local_to_temp: FxHashMap<reifier::Symbol, TempId>,
    variant_ids: FxHashMap<reifier::VariantItemType<'a>, usize>,
    variant_id_counter: usize,
    insns: Vec<AnyInsn<usize>>,
}

impl<'a> TacBuilder<'a> {
    fn tac(
        mut self,
        spec: bool,
        arg: Option<&reifier::Pattern>,
        body: &reifier::Expr<'a>,
    ) -> Option<TempId> {
        self.expr(body)
    }

    fn scope(&mut self, scope: &reifier::Scope<'a>) -> Option<TempId> {
        let mut res = None;

        for expr in &*scope.exprs {
            res = self.expr(expr);
        }

        if scope.discard {
            None
        } else {
            res
        }
    }

    fn expr(&mut self, expr: &reifier::Expr<'a>) -> Option<TempId> {
        let mut used_out_temp = false;
        let _out_temp_storage = self.new_temp();
        let mut out_temp = || {
            used_out_temp = true;
            _out_temp_storage
        };

        match &expr.kind {
            reifier::ExprKind::Scope(scope) => return self.scope(scope),
            reifier::ExprKind::Abstract { spec, arg, body } => todo!(),
            reifier::ExprKind::For {
                init,
                cond,
                afterthought,
                body,
            } => todo!(),
            reifier::ExprKind::Case {
                cond,
                on_true,
                on_false,
            } => todo!(),
            reifier::ExprKind::Tuple(items) => todo!(),
            reifier::ExprKind::StructuralEq(_, _) => todo!(),
            reifier::ExprKind::Binary(op, a, b) => todo!(),
            reifier::ExprKind::Unary(op, a) => todo!(),
            reifier::ExprKind::Call(base, arg) => {}
            reifier::ExprKind::Construct(sym, data) => {}
            reifier::ExprKind::Variant(name, data) => {
                let reifier::Type::Variant(items) = &expr.ty else {
                    panic!()
                };
                let [ty] = &**items else {
                    panic!()
                };
                let id = if let Some(&id) = self.variant_ids.get(ty) {
                    id
                } else {
                    let id = self.variant_id_counter;
                    self.variant_id_counter += 1;
                    self.variant_ids.insert(ty.clone(), id);
                    id
                };

                // steps
                // 1. allocate the memory (say this gives address a)
                // 2. store the id at address a
                // 3. store the ref to data at address a+8
            }
            reifier::ExprKind::Symbol(sym) => return Some(*self.local_to_temp.get(sym).unwrap()),
            reifier::ExprKind::Literal(lit) => match lit {
                &reifier::Literal::Boolean(b) => self.gen_insn(Insn::ConstI(DtSc {
                    dest: out_temp(),
                    cnst: b as u64,
                })),
                &reifier::Literal::Integer(i) => self.gen_insn(Insn::ConstI(DtSc {
                    dest: out_temp(),
                    cnst: i,
                })),
                &reifier::Literal::Float(f) => self.gen_insn(Insn::ConstF(DtSc {
                    dest: out_temp(),
                    cnst: f,
                })),
                _ => (),
            },
        }

        if used_out_temp {
            Some(_out_temp_storage)
        } else {
            None
        }
    }

    fn new_temp(&mut self) -> TempId {
        todo!()
    }

    fn gen_insn(&mut self, insn: Insn) {
        self.insns.push(AnyInsn::Insn(insn));
    }

    fn gen_ctrl(&mut self, ctrl: CtrlInsn<usize>) {
        self.insns.push(AnyInsn::Ctrl(ctrl));
    }
}
