use std::{cell::Cell, rc::Rc};

use crate::craftvm::common::OpType;

use super::{
    chunk::CrChunk,
    common::OpCode,
    value::CrValue,
};

pub fn disas(ch: &CrChunk) {
    //println!("== {title:?} ==");
    ch.each(disas_instr_typ);
}


pub fn disas_instr_typ(ch: &CrChunk, idx: usize, line: usize, ele: &OpType) {
    match ele {
        OpType::Simple(OpCode::OpCnst(cidx)) => {
            let v = ch.fetch_const(*cidx);
            // why does changing this to display drop the value ??
            println!("  {idx:04}  | {line:03} | {ele} '{v:?}'");
        }
        _ => {
            println!("  {idx:04}  | {line:03} | {ele}");
        }
    }
}

pub fn disas_instr(ch: &CrChunk, idx: usize, line: usize, ele: &OpCode) {
    match ele {
        OpCode::OpCnst(cidx) => {
            let v = ch.fetch_const(*cidx);
            println!("  {idx:04}  | {line:03} | {ele} '{v}'");
        }
        _ => {
            println!("  {idx:04}  | {line:03} | {ele}");
        }
    }
}

pub fn disas_stack(stck: &[Rc<Cell<CrValue>>], topidx: usize) {
    (topidx..0).for_each(|i| {
        let v = stck[i].get();
        println!("[ {v:?} ]");
    });
}
