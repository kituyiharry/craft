use crate::{chunk::Chunk, common::OpCode, debug::disas};

pub mod common;
pub mod chunk;
pub mod debug;

ocaml::import! {
    fn hello_world() -> String;
}

fn main() {
    // let gc = ocaml::runtime::init();

    let ch: Chunk = vec![ OpCode::OpReturn ];
    disas("test", 1, ch.iter());

    // unsafe {
    //     println!("hello_world =>: {}", hello_world(&gc).unwrap());
    // }
}
