use ocaml::Seq;

use crate::craftvm::vm::InterpretResult;

pub mod craftvm;

#[ocaml::func]
#[ocaml::sig("(Token.tokentype * int * int) Seq.t -> Compiler.compileresult")]
pub fn compile(t: Seq<(craftvm::scanner::CrTokenType, usize, usize)>) -> InterpretResult {
    let mut curl = 0;
    print!(" {curl} |");
    t.into_iter().flatten().for_each(|(t, l, _c)| {
        if curl == l {
            print!(" {t:?} ")
        } else {
            curl = l;
            print!("\n {l} | {t:?} ")
        }
    }); 
    println!();
    InterpretResult::InterpretOK
}
