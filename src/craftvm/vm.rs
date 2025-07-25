use std::{array, cell::RefCell, ops::{Add, Div, Mul, Sub}, rc::Rc};
use ocaml::Seq;

use crate::craftvm::compiler::{compile, CraftParser, TokSeqItem};
use super::{chunk::{CraftChunk}, common::OpCode, value::CraftValue};

#[derive(Default)]
#[derive(ocaml::ToValue, ocaml::FromValue)]
pub enum InterpretResult {
  #[default]
  InterpretExit,
  InterpretOK,
  InterpretBreak,
  InterpretCompileError,
  InterpretRuntimeError
}

pub struct CraftVm<const STACKSIZE: usize> {
    source: Rc<RefCell<CraftChunk>>,
    vstack: [CraftValue; STACKSIZE],
    stkptr: CraftValue, // stack pointer
    stkidx: usize,
}

impl<const STACK: usize> CraftVm<STACK> {

    pub fn new(ch: CraftChunk) -> Self {
        let vstck = array::from_fn::<_, STACK, _>( 
            |_idx| (CraftValue::default())
        );
        let stcki = 0;
        let stckp = vstck[stcki];
        Self {
            source: Rc::new(RefCell::new(ch)),
            vstack: vstck, 
            stkptr: stckp,
            stkidx: stcki,
        }
    }

    pub fn reset_stack(&mut self) {
        self.stkidx = 0;
        self.stkptr = self.vstack[0];
    }

    #[inline]
    fn push(&mut self, val: CraftValue) {
        self.vstack[self.stkidx] = val;
        self.stkidx += 1;
        self.stkptr = self.vstack[self.stkidx];
    }

    #[inline]
    fn pop(&mut self) -> CraftValue {
        if self.stkidx > 0 {
            self.stkidx -= 1;
        }
        self.stkptr = self.vstack[self.stkidx];
        self.stkptr
    }

    #[inline]
    fn binop(&mut self, f: fn(CraftValue, CraftValue) -> CraftValue) {
        let b = self.pop(); 
        let a = self.pop();
        let c = f(a, b);
        self.push(c);
    }

    pub fn warm(&mut self, chunk: CraftChunk) {
        self.reset_stack();
        self.source.replace(chunk);
    }

    pub fn run(&mut self) -> InterpretResult {

        #[cfg(feature = "vmtrace")]
        println!("== vm trace ==");

        // To avoid being told we are "modifying something immutable"
        let srclne = self.source.clone(); 
        let bsrc   = srclne.borrow_mut();
        let mut instrptr = bsrc.into_iter();

        // start vm thread
        loop {
            if let Some((_idx, _line, op)) = instrptr.next() {

                #[cfg(feature = "vmtrace")]
                super::debug::disas_instr(&bsrc, _idx, _line, op);

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
                        let val = bsrc.fetch_const(*idx);
                        self.push(val);
                    }
                    OpCode::OpNop => println!("Nop")
                }
            } else { 
                break InterpretResult::InterpretBreak;
            };
        }
    }
}

pub fn interpret<const S: usize>(vm: &mut CraftVm<S>, ts: Seq<TokSeqItem>) -> InterpretResult {
    let chunk:  CraftChunk   = CraftChunk::new(); 
    let parser: CraftParser = CraftParser::new(ts);
    if !compile(&chunk, &parser) {
       InterpretResult::InterpretCompileError
    } else {
        vm.warm(chunk); 
        vm.run()
    }
}
