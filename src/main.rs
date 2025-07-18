pub mod common;
pub mod chunk;
pub mod debug;
pub mod value;

use crate::{chunk::{CraftChunk}, common::{OpCode, OpType}, debug::disas};

ocaml::import! {
    fn hello_world() -> String;
}

fn main() {
    // let gc = ocaml::runtime::init();

    let mut ch: CraftChunk = CraftChunk::new(); 
    ch.add_const(1.2, 1);
    ch.insert(OpType::Simple(OpCode::OpReturn), 0);
    disas("test chunk", &ch);

    // unsafe {
    //     println!("hello_world =>: {}", hello_world(&gc).unwrap());
    // }
}
