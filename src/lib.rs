use std::sync::{Once};

use env_logger::{self, Env};
use ocaml::Seq;

use crate::craftvm::{
    chunk::CraftChunk,
    vm::{interpret, CraftVm, InterpretResult},
};

pub mod craftvm;

static INIT: Once = Once::new();

#[ocaml::func]
#[ocaml::sig("(Token.tokentype * int * int) Seq.t -> Compiler.compileresult")]
pub fn compile(t: Seq<(craftvm::scanner::CrTokenType, usize, usize)>) -> InterpretResult {
    // let mut curl = 0;
    INIT.call_once(|| { 
        env_logger::Builder::from_env(Env::default().default_filter_or("debug")).init();
    });
    let ch: CraftChunk = CraftChunk::new();
    let mut vm = CraftVm::<512>::new(ch);
    let r = interpret(&mut vm, t);
    log::info!("Finished with result {r:?}");
    r
}
