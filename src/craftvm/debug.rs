use std::{cell::Cell, rc::Rc};

use super::{
    chunk::{CrChunk, CraftChunkIter},
    common::OpCode,
    value::CrValue,
};

pub fn disas<'a>(title: &'a str, ch: &'a CrChunk, chnk: CraftChunkIter<'a>) {
    println!("== {title:?} ==");
    for (idx, line, ele) in chnk {
        disas_instr(ch, idx, line, ele);
    }
}

pub fn disas_instr<'a>(ch: &'a CrChunk, idx: usize, line: usize, ele: &'a OpCode) {
    match ele {
        OpCode::OpCnst(cidx) => {
            let v = ch.fetch_const(*cidx);
            println!("{idx:04} {line:03} {ele:?}  '{v:?}'");
        }
        _ => {
            println!("{idx:04}  |  {ele:?}");
        }
    }
}

pub fn disas_stack(stck: &[Rc<Cell<CrValue>>], topidx: usize) {
    (topidx..0).for_each(|i| {
        let v = stck[i].get();
        println!("[ {v:?} ]");
    });
}
