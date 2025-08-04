pub mod craftvm;
use craftvm::{
    chunk::CraftChunk,
    common::{OpCode, OpType},
    //compiler,
    vm::CraftVm,
};
use env_logger::Env;

use crate::craftvm::value::CraftValue;

const STACKMAX: usize = 256;

fn main() {

    env_logger::Builder::from_env(
        Env::default().default_filter_or("error")
    ).format_timestamp_nanos().init();

    let mut ch: CraftChunk = CraftChunk::new();

    ch.add_const(CraftValue::CrNumber(1.0), 1);
    ch.add_const(CraftValue::CrNumber(3.0), 1);
    ch.emit_byte(OpType::Simple(OpCode::OpAdd), 1);
    ch.add_const(CraftValue::CrNumber(5.0), 1);
    ch.emit_byte(OpType::Simple(OpCode::OpDiv), 1);
    ch.emit_byte(OpType::Simple(OpCode::OpNegate), 2);
    ch.emit_byte(OpType::Simple(OpCode::OpReturn), 2);

    let mut cvm = CraftVm::<STACKMAX>::new(ch);
    cvm.run();
}
