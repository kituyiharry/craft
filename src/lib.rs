#![allow(static_mut_refs)]
use std::sync::{Once};

use env_logger::{self, Env};
use ocaml::Seq;
use once_cell::unsync::Lazy;

use crate::craftvm::{
    vm::{interpret, CrVm, InterpretResult},
};

pub mod craftvm;

static INIT: Once = Once::new();
static mut CRVM: Lazy<CrVm<512>> = Lazy::new(CrVm::new);

#[ocaml::func]
#[ocaml::sig("(Token.tokentype * int * int) Seq.t -> Compiler.compileresult")]
pub fn compile(t: Seq<(craftvm::scanner::CrTokenType, usize, usize)>) -> InterpretResult {
    // let mut curl = 0;
    INIT.call_once(|| { 
        env_logger::Builder::from_env(
            Env::default().default_filter_or("error")
        ).format_timestamp(None).init();
    });
    let r = unsafe { interpret(&mut CRVM, t) };
    log::info!("Finished with result {r:?}");
    r
}


#[ocaml::func]
#[ocaml::sig("unit -> unit")]
pub fn reset_stack() -> () {
    unsafe{ CRVM.reset_stack() };
}

#[ocaml::func]
#[ocaml::sig("unit -> unit")]
pub fn debug_stack() -> () {
    unsafe{ CRVM.dump_stack() };
}

#[ocaml::func]
#[ocaml::sig("unit -> unit")]
pub fn debug_globals() -> () {
    unsafe{ CRVM.dump_globals() };
}

#[ocaml::func]
#[ocaml::sig("unit -> unit")]
pub fn debug_source() -> () {
    unsafe{ CRVM.dump_src() };
}
