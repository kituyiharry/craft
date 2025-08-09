use std::io::Write;
pub mod craftvm;
use chrono::Utc;
use craftvm::{
    chunk::CrChunk,
    common::{OpCode, OpType},
    //compiler,
    vm::CrVm,
};
use env_logger::Env;

use crate::craftvm::value::{CrObjVal, CrValue};

const STACKMAX: usize = 256;

fn main() {

    env_logger::Builder::from_env(
        Env::default().default_filter_or("error")
    ).format(|buf, record| {
            writeln!(
                buf,
                "{} [{}] - {}",
                Utc::now().timestamp(), // Unix timestamp
                record.level(),
                record.args()
            )
        })
    .init();

    let mut ch: CrChunk = CrChunk::new();

    ch.add_const(CrValue::CrObj(CrObjVal::from("st")), 1);
    ch.add_const(CrValue::CrObj(CrObjVal::from("ri")), 1);
    ch.emit_byte(OpType::Simple(OpCode::OpAdd), 1);
    ch.add_const(CrValue::CrObj(CrObjVal::from("ng")), 1);
    ch.emit_byte(OpType::Simple(OpCode::OpAdd), 1);
    ch.emit_byte(OpType::Simple(OpCode::OpReturn), 2);

    let mut cvm = CrVm::<STACKMAX>::new();
    cvm.warm(ch);
    cvm.run();
}
