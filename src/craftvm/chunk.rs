use chrono::offset;

use super::common::{OpCode, OpType};
use super::value::{ConstPool, CrValue};

// offset, line number and opcode
pub type Offset<'a> = (usize, usize, &'a OpCode);

pub struct CrChunk {
    instr: Vec<OpType>,
    cnsts: ConstPool,
    lines: Vec<usize>, // HINT: RLE encode this for better memory use
}

impl CrChunk {
    pub fn new() -> Self {
        Self {
            instr: vec![],
            lines: vec![],
            cnsts: ConstPool::new(),
        }
    }

    pub fn instrlen(&self) -> usize {
       self.instr.len()
    }

    pub fn mod_byte(&mut self, idx: usize, f: impl FnOnce(&mut OpType)) {
        f(&mut self.instr[idx])
    }

    pub fn emit_byte(&mut self, op: OpType, lineno: usize) {
        self.instr.push(op);
        self.lines.push(lineno);
    }

    pub fn add_obj(&mut self, obj: &str) -> usize {
        self.cnsts.intern(obj)
    }

    pub fn add_const(&mut self, val: CrValue, lineno: usize) -> usize {
        let idx = self.cnsts.insert(val);
        self.emit_byte(OpType::Simple(OpCode::OpCnst(idx)), lineno);
        idx
    }

    pub fn fetch_const(&self, idx: usize) -> &CrValue {
        self.cnsts.get(idx)
    }

    pub fn end_compiler(&mut self, line: usize) {
        self.instr.push(OpType::Simple(OpCode::OpReturn));
        self.lines.push(line);
    }
}

impl Default for CrChunk {
    fn default() -> Self {
        Self::new()
    }
}

pub struct CraftChunkIter<'a> {
    pub source: &'a CrChunk,
    pub offset: usize,
}

impl<'a> CraftChunkIter<'a> {

    pub fn jump(&mut self, offset: usize) {
        self.offset += offset
    }

}

impl<'a> IntoIterator for &'a CrChunk {
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
                },
                OpType::Jumper(ref op) => {
                    self.offset += 1;
                    Some((i, self.source.lines[i], op))
                },
            }
        }
    }
}
