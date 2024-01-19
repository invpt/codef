use crate::lowerer::*;

pub mod regalloc;

pub fn codegen(module: Module) -> Vec<u8> {
    todo!()
}

struct RvCodegen {
    buf: Vec<u8>,
}

impl RvCodegen {
    fn gen(&mut self, cfg: Cfg) {

    }
}

enum RvInsn {

}
