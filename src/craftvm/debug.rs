use std::{cell::Cell, rc::Rc};

use super::{chunk::{CraftChunk, CraftChunkIter}, common::OpCode, value::CraftValue};

pub fn disas<'a>(title: &'a str, ch: &'a CraftChunk, chnk: CraftChunkIter<'a>) {
    println!("== {title:?} ==");
    for (idx, line, ele) in chnk {
        disas_instr(ch, idx, line, ele);
    }
}

pub fn disas_instr<'a>(ch: &'a CraftChunk, idx: usize, line: usize, ele: &'a OpCode) {
    match ele {
        OpCode::OpConstant(cidx) => {
            let v = ch.fetch_const(*cidx);
            println!("{idx:04} {line:03} {ele:?}  '{v}'");
        },
        _  => {
            println!("{idx:04}  |  {ele:?}");
        }
    }
}

pub fn disas_stack(stck: &[Rc<Cell<CraftValue>>], topidx: usize) {
    (topidx..0).for_each(|i| {
        let v = stck[i].get();
        println!("[ {v} ]");
    });
}
