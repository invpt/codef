pub mod simplify;

use crate::lowerer::*;

pub fn optimize(module: &mut Module) {
    for (_, def) in &mut module.defs {
        match &mut def.value {
            Value::Function(cfg) => simplify::Simplify {}.run(cfg),
            _ => (),
        }
    }
}

trait OptimizationPass {
    fn run(&mut self, cfg: &mut Cfg);
}
