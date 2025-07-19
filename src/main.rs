pub mod common;
pub mod chunk;
pub mod debug;
pub mod value;
pub mod vm;

use crate::{chunk::CraftChunk, common::{OpCode, OpType}, vm::CraftVm};

ocaml::import! {
    fn hello_world() -> String;
}

const STACKMAX: usize = 256;

fn main() {
    // let gc = ocaml::runtime::init();

    let mut ch: CraftChunk = CraftChunk::new(); 
    ch.add_const(1.2, 1);
    ch.add_const(3.4, 1);
    ch.insert(OpType::Simple(OpCode::OpAdd),    1);
    ch.add_const(5.6, 1);
    ch.insert(OpType::Simple(OpCode::OpDiv),    1);
    ch.insert(OpType::Simple(OpCode::OpNegate), 2);
    ch.insert(OpType::Simple(OpCode::OpReturn), 2);

    //disas("test chunk", &ch, ch.into_iter());

    let chiter = ch.into_iter();

    let mut cvm = CraftVm::<STACKMAX>::new(&ch, chiter);
    cvm.run();

    // unsafe {
    //     println!("hello_world =>: {}", hello_world(&gc).unwrap());
    // }
}
