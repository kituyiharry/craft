use super::common::{OpCode, OpType};
use super::value::{ConstPool, CraftValue};

// offset, line number and opcode
pub type Offset<'a> = (usize, usize, &'a OpCode);

pub struct CraftChunk { 
    instr: Vec<OpType>,
    cnsts: ConstPool,
    lines: Vec<usize>, // HINT: RLE encode this for better memory use
}

impl CraftChunk  {

    pub fn new() -> Self {
        Self { 
            instr: vec![], 
            cnsts: ConstPool::new(), 
            lines: vec![], 
        }
    }

    pub fn emit_byte(&mut self, op: OpType, lineno: usize) {
        self.instr.push(op);
        self.lines.push(lineno);
    }

    pub fn add_const(&mut self, val: CraftValue, lineno: usize) {
        let idx = self.cnsts.insert(val);
        self.emit_byte(OpType::Simple(OpCode::OpConstant(idx)), lineno);
    }

    pub fn fetch_const(&self, idx: usize) -> CraftValue {
        self.cnsts.get(idx)
    }

    pub fn end_compiler(&mut self, line: usize) {
        self.instr.push(OpType::Simple(OpCode::OpReturn));
        self.lines.push(line);
    }

}

impl Default for CraftChunk {
    fn default() -> Self {
        Self::new()
    }
}

pub struct CraftChunkIter<'a> {
    pub source: &'a CraftChunk,
    offset:     usize,
}

impl<'a> IntoIterator for &'a CraftChunk {
    type Item = Offset<'a>;
    type IntoIter = CraftChunkIter<'a>;
    fn into_iter(self) -> Self::IntoIter {
        CraftChunkIter {
            source: self,
            offset: 0,
        }
    }
}

impl<'a> Iterator for CraftChunkIter<'a> {
    type Item = Offset<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.offset >= self.source.instr.len() {
            None
        } else {
            let i = self.offset;
            match self.source.instr[self.offset] {
                OpType::Simple(ref op) => {
                    self.offset += 1;
                    Some((i, self.source.lines[i], op))
                }
            }
        }
    }
}

