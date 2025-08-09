use std::sync::{Once};

use env_logger::{self, Env};
use ocaml::Seq;

use crate::craftvm::{
    vm::{interpret, CrVm, InterpretResult},
};

pub mod craftvm;

static INIT: Once = Once::new();

#[ocaml::func]
#[ocaml::sig("(Token.tokentype * int * int) Seq.t -> Compiler.compileresult")]
pub fn compile(t: Seq<(craftvm::scanner::CrTokenType, usize, usize)>) -> InterpretResult {
    // let mut curl = 0;
    INIT.call_once(|| { 
        env_logger::Builder::from_env(
            Env::default().default_filter_or("error")
        ).format_timestamp_millis().init();
    });
    let mut vm = CrVm::<512>::new();
    let r = interpret(&mut vm, t);
    log::info!("Finished with result {r:?}");
    r
}
