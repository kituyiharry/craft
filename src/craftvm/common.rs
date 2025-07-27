use std::fmt::Display;

#[derive(Default, Debug)]
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

impl Display for OpCode  {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::OpNop => {
                write!(f, "nop")
            },
            OpCode::OpReturn => {
                write!(f, "return")
            },
            OpCode::OpConstant(num) => {
                write!(f, "const {num}")
            },
            OpCode::OpNegate => {
                write!(f, " (-) ")
            },
            OpCode::OpSub => {
                write!(f, " [-] ")
            },
            OpCode::OpAdd => {
                write!(f, " (+) ")
            },
            OpCode::OpMult => { 
                write!(f, " (*) ")
            },
            OpCode::OpDiv => {
                write!(f, " (/) ")
            },
        }
    }
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

