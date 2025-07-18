use crate::{chunk::{CraftChunk}, common::OpCode};

pub fn disas<'a>(title: &'a str, ch: &'a CraftChunk) {
    let chnk = ch.into_iter();
    println!("== {title:?} ==");
    for (idx, line, ele) in chnk {
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
}
