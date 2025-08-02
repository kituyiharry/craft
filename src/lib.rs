use ocaml::Seq;

use crate::craftvm::{
    chunk::CraftChunk,
    vm::{interpret, CraftVm, InterpretResult},
};

pub mod craftvm;

#[ocaml::func]
#[ocaml::sig("(Token.tokentype * int * int) Seq.t -> Compiler.compileresult")]
pub fn compile(t: Seq<(craftvm::scanner::CrTokenType, usize, usize)>) -> InterpretResult {
    // let mut curl = 0;
    let ch: CraftChunk = CraftChunk::new();
    let mut vm = CraftVm::<512>::new(ch);
    // print!(" {curl} |");
    // t.into_iter().flatten().for_each(|(t, l, _c)| {
    //     if curl == l {
    //         print!(" {t:?} ")
    //     } else {
    //         curl = l;
    //         print!("\n {l} | {t:?} ")
    //     }
    // });
    println!("we compiled!!");
    let r = interpret(&mut vm, t);
    println!("we got {r:?}!!");
    r
}
