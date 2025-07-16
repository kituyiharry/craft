use crate::common::OpCode;

pub fn disas<'a, 'b>(title: &'a str, size: usize, chnk: impl Iterator<Item = &'b OpCode>) {
    println!("== {title:?} ==");
    for (ele, idx) in chnk.zip(0..size) {
        println!("{idx:04} {ele:?}");
    }
}
