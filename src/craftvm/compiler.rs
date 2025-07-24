use crate::craftvm::{chunk::CraftChunk, scanner::CrTokenType};

pub struct CraftParser {
    pub current : CrTokenType,
    pub previous: CrTokenType,
    pub had_err : bool, 
    pub panic_md: bool, 
}

pub fn compile(_chunk: &'_ CraftChunk) -> bool {
    false
}
