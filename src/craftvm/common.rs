#[derive(Debug)]
#[derive(Default)]
pub enum OpCode {
    #[default]
    OpNop,
    OpReturn,
    OpConstant(usize),
    // Arithmetic
    OpNegate,
    OpSub,
    OpAdd, 
    OpMult,
    OpDiv,
}

#[derive(Debug)]
pub enum OpType {
    Simple(OpCode)
}


impl Default for OpType {
    fn default() -> Self {
        OpType::Simple(OpCode::default())
    }
}

