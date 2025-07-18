use crate::common::{OpCode, OpType};
use crate::value::{ConstPool, CraftValue};

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

    pub fn insert(&mut self, op: OpType, lineno: usize) {
        self.instr.push(op);
        self.lines.push(lineno);
    }

    pub fn add_const(&mut self, val: CraftValue, lineno: usize) {
        let idx = self.cnsts.insert(val);
        self.insert(OpType::Simple(OpCode::OpConstant(idx)), lineno);
    }

    pub fn fetch_const(&self, idx: usize) -> CraftValue {
        self.cnsts.get(idx)
    }

}

impl Default for CraftChunk {
    fn default() -> Self {
        Self::new()
    }
}


pub struct CraftChunkIter<'chunk> {
    offset: usize,
    stream: (&'chunk Vec<OpType>, &'chunk Vec<usize>),
}

impl<'a> IntoIterator for &'a CraftChunk {
    type Item = Offset<'a>;
    type IntoIter = CraftChunkIter<'a>;
    fn into_iter(self) -> Self::IntoIter {
        CraftChunkIter {
            stream: (&self.instr, &self.lines),
            offset: 0,
        }
    }
}

impl<'a> Iterator for CraftChunkIter<'a> {
    type Item = Offset<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.offset >= self.stream.0.len() {
            None
        } else {
            let i = self.offset;
            match self.stream.0[self.offset] {
                OpType::Simple(ref op) => {
                    self.offset += 1;
                    Some((i, self.stream.1[i], op))
                }
            }
        }
    }
}

