use std::{fmt::Display, hash::Hash};

#[derive(Default, Debug, Hash, Clone)]
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
    // Conds
    OpJumpIfFalse(usize),
    OpJump(usize),
    // Loops
    OpLoop(usize),
    // Function calls
    OpCall(String, usize),
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
                write!(f, "loadcnst idx:{num}")
            }
            OpCode::OpNegate => {
                write!(f, "neg(-)")
            }
            OpCode::OpSub => {
                write!(f, "[-]")
            }
            OpCode::OpAdd => {
                write!(f, "(+)")
            }
            OpCode::OpMult => {
                write!(f, "(*)")
            }
            OpCode::OpDiv => {
                write!(f, "(/)")
            }
            OpCode::OpTrue => {
                write!(f, "true")
            }
            OpCode::OpFalse => {
                write!(f, "false")
            }
            OpCode::OpNil => {
                write!(f, "nil")
            }
            OpCode::OpNot => {
                write!(f, "!")
            }
            OpCode::OpEqual => {
                write!(f, "=")
            }
            OpCode::OpGreater => {
                write!(f, ">")
            }
            OpCode::OpLess => {
                write!(f, "<")
            }
            OpCode::OpPrint => {
                write!(f, "@print")
            }
            OpCode::OpPop => {
                write!(f, "<-pop")
            },
            OpCode::OpDefGlob(x) => {
                write!(f, "@declglob{{name:{x}}}")
            }, 
            OpCode::OpGetGlob(x) => {
                write!(f, "@ftchglob{{name:{x}}}")
            }, 
            OpCode::OpSetGlob(x) => {
                write!(f, "@setglvar{{name:{x}}}")
            }, 
            OpCode::OpGetLoc(s, u) => {
                write!(f, "@getlcvar{{{s}:idx:{u}}}")
            },
            OpCode::OpSetLoc(s, u) => {
                write!(f, "@setlcvar{{{s}:idx:{u}}}")
            }, 
            OpCode::OpJumpIfFalse(offset) => {
                write!(f, "@jump-if-false({offset})")
            },
            OpCode::OpJump(offset) => {
                write!(f, "@jump({offset})")
            },
            OpCode::OpLoop(location) => {
                write!(f, "@loop(locidx={location})")
            },
            OpCode::OpCall(fname, args) => {
                write!(f, "@call:{fname}(argc={args})")
            }
        }
    }
}

#[derive(Debug, Hash, Clone)]
pub enum OpType {
    Simple(OpCode),
    Jumper(OpCode),
}

impl Display for OpType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpType::Simple(op_code) => {
                write!(f, "{op_code}")
            },
            OpType::Jumper(op_code) => {
                write!(f, "{op_code}    --jump-->")
            },
        }
    }
}

impl Default for OpType {
    fn default() -> Self {
        OpType::Simple(OpCode::default())
    }
}
