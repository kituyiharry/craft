pub mod craftvm;
use craftvm::{
    chunk::CraftChunk,
    common::{OpCode, OpType},
    //compiler,
    vm::CraftVm,
};

const STACKMAX: usize = 256;

fn main() {
    let mut ch: CraftChunk = CraftChunk::new();

    ch.add_const(1.2, 1);
    ch.add_const(3.4, 1);
    ch.emit_byte(OpType::Simple(OpCode::OpAdd), 1);
    ch.add_const(5.6, 1);
    ch.emit_byte(OpType::Simple(OpCode::OpDiv), 1);
    ch.emit_byte(OpType::Simple(OpCode::OpNegate), 2);
    ch.emit_byte(OpType::Simple(OpCode::OpReturn), 2);

    //let _rule = compiler::PARSERULES.lock().unwrap().get(craftvm::scanner::CrTokenType::CrDot.order());

    //disas("test chunk", &ch, ch.into_iter());
    //let chiter = ch.iter();

    let mut cvm = CraftVm::<STACKMAX>::new(ch);
    cvm.run();
}
