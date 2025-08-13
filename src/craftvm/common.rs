use std::{fmt::Display};

#[derive(Default, Debug)]
pub enum OpCode {
    #[default]
    OpNop,
    OpPop,
    //
    OpReturn,
    OpCnst(usize),
    // Arithmetic
    OpNegate,
    OpSub,
    OpAdd,
    OpMult,
    OpDiv,
    OpTrue,
    OpFalse,
    OpNil,
    OpNot,
    OpEqual, 
    // we don't use equal versions of these to maximise instruction reuse
    // The expression a != b has the same semantics as !(a == b), so the compiler 
    // is free to compile the former as if it were the latter.
    // Instead of a dedicated OP_NOT_EQUAL instruction, it can output an OP_EQUAL followed by an OP_NOT.
    // Likewise, a <= b is the same as !(a > b) and a >= b is !(a < b)
    // Thus, we only need three new instructions.
    OpGreater,
    OpLess,
    // vars 
    OpPrint,
    // Global vars
    OpDefGlob(usize),
    OpGetGlob(String),
    OpSetGlob(String),
    // Local Vars
    OpGetLoc(String, usize),
    OpSetLoc(String, usize),
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpCode::OpNop => {
                write!(f, "nop")
            }
            OpCode::OpReturn => {
                write!(f, "return")
            }
            OpCode::OpCnst(num) => {
                write!(f, "const {num}")
            }
            OpCode::OpNegate => {
                write!(f, " (-) ")
            }
            OpCode::OpSub => {
                write!(f, " [-] ")
            }
            OpCode::OpAdd => {
                write!(f, " (+) ")
            }
            OpCode::OpMult => {
                write!(f, " (*) ")
            }
            OpCode::OpDiv => {
                write!(f, " (/) ")
            }
            OpCode::OpTrue => {
                write!(f, " (true) ")
            }
            OpCode::OpFalse => {
                write!(f, " (false) ")
            }
            OpCode::OpNil => {
                write!(f, " nil ")
            }
            OpCode::OpNot => {
                write!(f, " ! ")
            }
            OpCode::OpEqual => {
                write!(f, " = ")
            }
            OpCode::OpGreater => {
                write!(f, " > ")
            }
            OpCode::OpLess => {
                write!(f, " < ")
            }
            OpCode::OpPrint => {
                write!(f, " @print ")
            }
            OpCode::OpPop => {
                write!(f, " []pop ")
            },
            OpCode::OpDefGlob(x) => {
                write!(f, " @declrglobal{{idx:{x}}} ")
            }, 
            OpCode::OpGetGlob(x) => {
                write!(f, " @fetchglobal{{idx:{x}}} ")
            }, 
            OpCode::OpSetGlob(x) => {
                write!(f, " @setvrglobal{{idx:{x}}} ")
            }, 
            OpCode::OpGetLoc(s, u) => {
                write!(f, " @getvrlocal{{{s}:idx:{u}}} ")
            },
            OpCode::OpSetLoc(s, u) => {
                write!(f, " @setvrlocal{{{s}:idx:{u}}} ")
            }
        }
    }
}

#[derive(Debug)]
pub enum OpType {
    Simple(OpCode),
}

impl Default for OpType {
    fn default() -> Self {
        OpType::Simple(OpCode::default())
    }
}
