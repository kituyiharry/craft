use ocaml::Seq;
use std::{
    array,
    cell::{Cell, RefCell},
    ops::{Add, Div, Mul, Sub},
    rc::Rc,
};

use crate::craftvm::value::are_same_type;

use super::compiler::{compile, CraftParser, TokSeqItem};
use super::{chunk::CraftChunk, common::OpCode, value::CraftValue};

#[derive(Default, Debug, ocaml::ToValue, ocaml::FromValue)]
pub enum InterpretResult {
    #[default]
    InterpretExit,
    InterpretOK,
    InterpretBreak,
    InterpretCompileError,
    InterpretRuntimeError,
}

pub struct CraftVm<const STACKSIZE: usize> {
    source: Rc<RefCell<CraftChunk>>,
    vstack: [Cell<CraftValue>; STACKSIZE],
    stkptr: *mut CraftValue, // stack pointer
    stkidx: usize,
    // sentinel for breaking run loop from other methods ;-) - bad design lol
    iserr : bool,
}

#[allow(dead_code)]
impl<const STACK: usize> CraftVm<STACK> {
    pub fn new(ch: CraftChunk) -> Self {
        log::debug!("Creating new VM");
        let vstck = array::from_fn::<_, STACK, _>(|_idx| Cell::new(CraftValue::CrNil));
        let stcki = 0;
        let stckp = vstck[stcki].as_ptr();
        Self {
            source: Rc::new(RefCell::new(ch)),
            vstack: vstck,
            stkptr: stckp,
            stkidx: stcki,
            iserr:  false,
        }
    }

    pub fn reset_stack(&mut self) {
        log::debug!("Stack Reset");
        self.stkidx = 0;
        self.stkptr = self.vstack[0].as_ptr();
    }

    #[inline]
    fn push(&mut self, val: CraftValue) {
        self.vstack[self.stkidx].set(val);
        self.stkidx += 1;
        self.stkptr = self.vstack[self.stkidx].as_ptr();
        log::debug!("pushed value {val:?} at {}", self.stkidx);
    }

    //#[inline]
    fn pop(&mut self) -> *mut CraftValue {
        self.stkidx -= 1;
        self.stkptr = self.vstack[self.stkidx].as_ptr();
        unsafe {
            log::debug!("popped value {:?} at ({})", *(self.stkptr), self.stkidx);
        };
        self.stkptr
    }

    #[inline]
    fn binop(&mut self, f: fn(CraftValue, CraftValue) -> CraftValue) {
        unsafe {
            let b = self.pop();
            let a = self.pop();
            if are_same_type(*a, *b) {
                self.push(f(*a, *b));
            } else {
                log::error!("Attempt binary op on mistyped values!");
                self.iserr = true
            }
        }
    }

    // Too overpowered as an API, just use matches after popping!
    // fn peek<T: FnMut(*mut CraftValue)>(&mut self, dist: usize, mut f: T) {
    //    f(self.vstack[self.stkidx-dist].as_ptr())
    // }

    fn dump(&self) {
        log::debug!("========== stack =============");
        let mut pos = self.stkidx;
        while pos > 0 {
            log::debug!("\t {pos} ==> {:?}", self.vstack[pos]);
            pos-=1;
        }
        log::debug!("\t {pos} ==> {:?}", self.vstack[pos]);
        log::debug!("========== stack =============");
    }

    pub fn warm(&mut self, chunk: CraftChunk) {
        self.reset_stack();
        self.source.replace(chunk);
    }

    pub fn run(&mut self) -> InterpretResult {
        log::debug!("== vm exec ==");

        // To avoid being told we are "modifying something immutable"
        let srclne = self.source.clone();
        let bsrc   = srclne.borrow_mut();
        let mut instrptr = bsrc.into_iter().peekable();

        // start vm thread
        loop {
            if let Some((_idx, _line, op)) = instrptr.next() {
                if log::log_enabled!(log::Level::Debug) {
                    super::debug::disas_instr(&bsrc, _idx, _line, op);
                }

                // TODO: in the book, they peek before executing
                // In the book some instructions are 'merged' as well so we have to 
                // look ahead in this version as well. Its best to just implement all instructions 
                // directly for simplicity for future reference i.e for >=, <= .. etc
                match op {
                    OpCode::OpReturn => {
                        let val = self.pop();
                        unsafe { println!("{:?}", *val) };
                        return InterpretResult::InterpretOK;
                    }
                    OpCode::OpNegate => {
                        unsafe {
                            let pop = self.pop();
                            if matches!(*pop, CraftValue::CrNumber(_)) {
                                self.push(-(*pop)) ;
                            } else {
                                log::error!("expected a number at stack top");
                                self.dump();
                                return InterpretResult::InterpretCompileError;
                            }
                        }
                    }
                    OpCode::OpNot  => {
                        unsafe {
                            let pop = self.pop();
                            if matches!(*pop, CraftValue::CrBool(_) | CraftValue::CrNil) {
                                self.push(!(*pop)) ;
                            } else {
                                log::error!("expected a bool at stack top");
                                self.dump();
                                return InterpretResult::InterpretCompileError;
                            }
                        }
                    },
                    OpCode::OpSub  => self.binop(CraftValue::sub),
                    OpCode::OpAdd  => self.binop(CraftValue::add),
                    OpCode::OpMult => self.binop(CraftValue::mul),
                    OpCode::OpDiv  => self.binop(CraftValue::div),
                    OpCode::OpCnst(idx) => self.push(*bsrc.fetch_const(*idx)),
                    OpCode::OpNop   => log::debug!("Nop instruction"),
                    OpCode::OpTrue  => self.push(CraftValue::CrBool(true)),
                    OpCode::OpFalse => self.push(CraftValue::CrBool(false)),
                    OpCode::OpNil   => self.push(CraftValue::CrNil),
                    OpCode::OpEqual => {
                        let a = self.pop();
                        let b = self.pop();
                        unsafe  {
                            if are_same_type(*a, *b) {
                                log::debug!("comparing {:?} == {:?}", *a, *b);
                                self.push(CraftValue::CrBool(*a == *b));
                            } else {
                                log::error!("attempt to compare non-matched types");
                                self.dump();
                                break InterpretResult::InterpretCompileError;
                            }
                        }
                    },
                    OpCode::OpGreater => {
                        self.binop(|l, r| {
                            log::debug!("comparing {l:?} > {r:?}");
                            CraftValue::CrBool(l > r)
                        });
                    },
                    OpCode::OpLess => {
                        self.binop(|l, r| {
                            log::debug!("comparing {l:?} < {r:?}");
                            CraftValue::CrBool(l < r)
                        });
                    },
                }
                if self.iserr {
                    self.dump();
                    break InterpretResult::InterpretCompileError; 
                }
            } else {
                break InterpretResult::InterpretOK;
            };
        }
    }
}

pub fn interpret<'a, const S: usize>(
    vm: &mut CraftVm<S>,
    ts: Seq<TokSeqItem<'a>>,
) -> InterpretResult {
    // maybe i'll chain the Seqs ??
    let chunk = RefCell::new(CraftChunk::new());
    let mut parser: CraftParser = CraftParser::new(ts, chunk);
    if !compile(&mut parser) {
        log::error!("Compilation Error");
        InterpretResult::InterpretCompileError
    } else {
        log::info!("executing");
        vm.warm(parser.chnk.into_inner());
        vm.run()
    }
}
