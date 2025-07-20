use std::{array, cell::Cell, ops::{Add, Mul, Div, Sub}, rc::Rc};
use super::{chunk::{CraftChunk, CraftChunkIter, Offset}, common::OpCode, value::CraftValue};

#[derive(Default)]
pub enum InterpretResult {
  InterpretOK,
  #[default]
  InterpretExit,
  InterpretCompileError,
  InterpretRuntimeError
}


pub struct CraftVm<'a, const STACKSIZE: usize> {
    insptr: Option<Offset<'a>>,   // instruction pointer
    chunks: CraftChunkIter<'a>,
    source: &'a CraftChunk,

    vstack: [Rc<Cell<CraftValue>>; STACKSIZE],
    stkptr: Rc<Cell<CraftValue>>, // stack pointer
    stkidx: usize,
}

impl<'a, const STACK: usize> CraftVm<'a, STACK> {
    pub fn new(ch: &'a CraftChunk, mut chitr: CraftChunkIter<'a>) -> Self {
        let instr = chitr.next();
        let vstck = array::from_fn::<_, STACK, _>( |_idx|  Rc::new(Cell::new(CraftValue::default())));
        let stcki = 0;
        let stckp = (vstck[stcki]).clone();
        Self {
            chunks: chitr,
            insptr: instr,
            source: ch,
            vstack: vstck, 
            stkptr: stckp,
            stkidx: stcki,
        }
    }

    pub fn reset_stack(&mut self) {
        self.stkidx = 0;
        self.stkptr = (self.vstack[0]).clone();
    }

    #[inline]
    fn push(&mut self, val: CraftValue) {
        self.stkptr.set(val);
        self.stkidx += 1;
        self.stkptr = self.vstack[self.stkidx].clone();
    }

    #[inline]
    fn pop(&mut self) -> CraftValue {
        if self.stkidx > 0 {
            self.stkidx -= 1;
        }
        self.stkptr = self.vstack[self.stkidx].clone();
        self.stkptr.get()
    }

    #[inline]
    fn binop(&mut self, f: fn(CraftValue, CraftValue) -> CraftValue) {
        let b = self.pop(); 
        let a = self.pop();
        let c = f(a, b);
        self.push(c);
    }

    pub fn run(&mut self) -> InterpretResult {
        println!("== vm exec ==");
        // start vm thread
        loop {
            if let Some((_idx, _line, op)) = self.insptr {

                #[cfg(feature = "vmtrace")]
                super::debug::disas_instr(self.source, _idx, _line, op);

                match op {
                    OpCode::OpReturn =>
                    {
                        let val = self.pop();
                        println!("popped value: {val}");
                        return InterpretResult::InterpretOK
                    },
                    OpCode::OpNegate =>
                    {
                        let pop = self.pop();
                        self.push(-pop);
                    },
                    OpCode::OpSub  => self.binop(f64::sub),
                    OpCode::OpAdd  => self.binop(f64::add),
                    OpCode::OpMult => self.binop(f64::mul),
                    OpCode::OpDiv  => self.binop(f64::div),
                    OpCode::OpConstant(idx) => 
                    {
                        let val = self.source.fetch_const(*idx);
                        self.push(val);
                    }
                    OpCode::OpNop => println!("Nop")
                }
            } else { 
                break InterpretResult::default();
            };
            self.insptr = self.chunks.next();
        }
    }
}

