#![allow(static_mut_refs)]
use ocaml::Seq;
use once_cell::unsync::Lazy;
use std::{
    array, cell::{Cell, RefCell}, collections::HashMap, ops::{Add, Div, Mul, Sub}, rc::Rc
};

use crate::craftvm::{debug, value::are_same_type};
use super::compiler::{compile, CraftParser, TokSeqItem};
use super::{chunk::CrChunk, common::OpCode, value::CrValue};

#[derive(Default, Debug, ocaml::ToValue, ocaml::FromValue)]
pub enum InterpretResult {
    #[default]
    InterpretExit,
    InterpretOK,
    InterpretBreak,
    InterpretCompileError,
    InterpretRuntimeError,
}

//                 length, capacity, ptr
type CrAllocData = (usize, usize, *const u8);
pub static mut CRALLOCA: Lazy<Vec<CrAllocData>> = Lazy::new(||{
    Vec::with_capacity(64)
});

// drops vector items
pub(crate) unsafe fn free() {
    CRALLOCA.iter().for_each(|(t, c, p)| {
        log::debug!("freeing at {:?}", *p);
        drop(Vec::from_raw_parts(*(p) as *mut u8, *t, *c));
    });
    CRALLOCA.truncate(0);
}

pub(crate) unsafe fn alloca(bytes: Vec<u8>) -> (usize, *const u8) {
    let tot = bytes.len();
    let cap = bytes.capacity();
    let ptr = Vec::leak(bytes).as_ptr();
    CRALLOCA.push((tot, cap, ptr));
    (tot, ptr)
}

pub struct CrVm<const STACKSIZE: usize> {
    source: Rc<RefCell<CrChunk>>,
    vstack: [Cell<CrValue>; STACKSIZE],
    stkptr: *mut CrValue, // stack pointer
    stkidx: usize,
    global: HashMap<String, Rc<Cell<CrValue>>>,
    // sentinel for breaking run loop from other methods ;-) - bad design lol
    iserr : bool,
    // objects are tracked in alloca and free
}

impl<const S: usize> Drop for CrVm<S> {
    fn drop(&mut self) {
        unsafe { 
            free(); 
        };
    }
}

#[allow(dead_code)]
impl<const STACK: usize> CrVm<STACK> {
    pub fn new() -> Self {
        log::debug!("Creating new VM");
        let vstck = array::from_fn::<_, STACK, _>(|_idx| Cell::new(CrValue::CrNil));
        let stcki = 0;
        let stckp = vstck[stcki].as_ptr();
        Self {
            source: Rc::new(RefCell::new(CrChunk::new())),
            vstack: vstck,
            stkptr: stckp,
            stkidx: stcki,
            global: HashMap::new(),
            iserr:  false,
        }
    }

    pub fn reset_stack(&mut self) {
        log::debug!("Stack Reset");
        self.stkidx = 0;
        self.stkptr = self.vstack[0].as_ptr();
    }

    #[inline]
    fn push(&mut self, val: CrValue) {
        self.vstack[self.stkidx].set(val);
        self.stkidx += 1;
        self.stkptr = self.vstack[self.stkidx].as_ptr();
        log::debug!("pushed value {val:?} at {}", self.stkidx-1);
    }

    #[inline]
    fn pop(&mut self) -> *mut CrValue {
        // TODO: use asserts
        if self.stkidx > 0 {
            self.stkidx -= 1;
            self.stkptr = self.vstack[self.stkidx].as_ptr();
            unsafe {
                log::debug!("popped value {:?} at ({})", *(self.stkptr), self.stkidx+1);
            };
        }
        self.stkptr
    }

    #[inline]
    fn binop(&mut self, f: fn(CrValue, CrValue) -> CrValue) {
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

    // make sure to restore locally popped values
    pub fn dump_stack(&self) {
        println!("============= stack ================");
        let mut pos = self.stkidx;
        while pos > 0 {
            println!("\t {pos} ==> {:?}", self.vstack[pos]);
            pos-=1;
        }
        println!("\t{pos} ==> {:?}", self.vstack[pos]);
        println!("============= stack ================");
    }

    pub fn dump_globals(&self) {
        println!("============= globals ================");
        self.global.iter().for_each(|(k, v)| {
            println!("\t{k} => {}", v.clone().get())
        });
        println!("============= globals ================");
    }

    pub fn dump_src(&self) {
        println!("============= source =============");
        let bsrc   = self.source.borrow();
        debug::disas(&self.source.clone().borrow(), bsrc.into_iter());
        println!("============= source =============");
    }

    pub fn warm(&mut self, chunk: CrChunk) {
        self.reset_stack();
        self.source.replace(chunk);
    }

    pub fn run(&mut self) -> InterpretResult {
        log::debug!("== vm exec ==");

        // To avoid being told we are "modifying something immutable"
        let srclne = self.source.clone();
        let bsrc   = srclne.borrow_mut();
        let mut instrptr = bsrc.into_iter();

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
                    OpCode::OpNop => log::debug!("Nop instruction"),
                    OpCode::OpPop => { self.pop(); },
                    OpCode::OpReturn => {
                                        //if self.stkidx != 0 {
                                            //let val = self.pop();
                                            //unsafe { println!("{:?}", *val) };
                                        //}
                                        return InterpretResult::InterpretOK;
                                    }
                    OpCode::OpNegate => {
                                        unsafe {
                                            let pop = self.pop();
                                            if matches!(*pop, CrValue::CrNumber(_)) {
                                                self.push(-(*pop)) ;
                                            } else {
                                                self.push(*pop);
                                                log::error!("expected a number at stack top");
                                                self.dump_stack();
                                                return InterpretResult::InterpretCompileError;
                                            }
                                        }
                                    }
                    OpCode::OpNot  => {
                                        unsafe {
                                            let pop = self.pop();
                                            if matches!(*pop, CrValue::CrBool(_) | CrValue::CrNil) {
                                                self.push(!(*pop)) ;
                                            } else {
                                                self.push(*pop);
                                                log::error!("expected a bool-like at stack top");
                                                self.dump_stack();
                                                return InterpretResult::InterpretCompileError;
                                            }
                                        }
                                    },
                    OpCode::OpSub  => self.binop(CrValue::sub),
                    OpCode::OpAdd  => self.binop(CrValue::add),
                    OpCode::OpMult => self.binop(CrValue::mul),
                    OpCode::OpDiv  => self.binop(CrValue::div),
                    OpCode::OpCnst(idx) => self.push(*bsrc.fetch_const(*idx)),
                    OpCode::OpTrue  => self.push(CrValue::CrBool(true)),
                    OpCode::OpFalse => self.push(CrValue::CrBool(false)),
                    OpCode::OpNil   => self.push(CrValue::CrNil),
                    OpCode::OpEqual => {
                                        let a = self.pop();
                                        let b = self.pop();
                                        unsafe  {
                                            if are_same_type(*a, *b) {
                                                log::debug!("comparing {:?} == {:?}", *a, *b);
                                                self.push(CrValue::CrBool(*a == *b));
                                            } else {
                                                log::error!("attempt to compare non-matched types");
                                                self.push(*b);
                                                self.push(*a);
                                                self.dump_stack();
                                                break InterpretResult::InterpretCompileError;
                                            }
                                        }
                                    },
                    OpCode::OpGreater => {
                                        self.binop(|l, r| {
                                            log::debug!("comparing {l:?} > {r:?}");
                                            CrValue::CrBool(l > r)
                                        });
                                    },
                    OpCode::OpLess => {
                                        self.binop(|l, r| {
                                            log::debug!("comparing {l:?} < {r:?}");
                                            CrValue::CrBool(l < r)
                                        });
                                    },
                    OpCode::OpPrint => {
                                        unsafe { 
                                            println!("{:}", *(self.pop())) 
                                        }
                                    }
                    OpCode::OpDefGlob(idx) => {
                        // likely a string object
                        // This code doesn’t check to see if the key is already in the table. 
                        // Lox is pretty lax with global variables and lets you redefine them without error. 
                        // That’s useful in a REPL session, so the VM supports that 
                        // by simply overwriting the value if the key happens to already be in the hash table.
                        
                        // get the var name as string
                        let identobj = bsrc.fetch_const(*idx).to_string();
                        unsafe { 
                            let v = *self.pop();
                            self.global.insert(identobj, Rc::new(Cell::new(v))); 
                        }
                    },
                    OpCode::OpGetGlob(g) => {
                        match self.global.get(g) {
                            Some(v) => {
                                self.push(v.clone().get());
                            },
                            None    => {
                                log::error!("Undefined global variable: {g}");
                                self.dump_stack();
                                break InterpretResult::InterpretCompileError
                            }
                        }
                    },
                    OpCode::OpSetGlob(s) => {
                        let nv = self.pop();
                        match self.global.entry(s.clone()) {
                            std::collections::hash_map::Entry::Occupied(o) => {
                                unsafe {
                                    o.get().set(*nv);
                                }
                            },
                            std::collections::hash_map::Entry::Vacant(_v) => {
                                log::error!("tried to set undefined variable {s}");
                                self.dump_stack();
                                break InterpretResult::InterpretCompileError
                            }
                        }
                    },
                    OpCode::OpGetLoc(s, n) => {
                        log::debug!("getting local `{s}` at stack pos: {}", *n);
                        self.push(self.vstack[*n].get());
                    },
                    OpCode::OpSetLoc(s, n) => {
                        log::debug!("setting local `{s}` at stack pos: {}", *n);
                        let v = self.pop();
                        unsafe {
                            self.vstack[*n].set(*v);
                        }
                    },
                    // unlike the book i don't pop the condition ??
                    // not sure the effect :-D
                    OpCode::OpJumpIfFalse(o) => {
                        unsafe {
                            if let CrValue::CrBool(n) = *self.pop() {
                                if !n {
                                    instrptr.jump(*o);
                                }
                            } else {
                                // all other values are falsey!!
                                instrptr.jump(*o);
                            }
                        }
                    },
                    OpCode::OpJump(o) => {
                        instrptr.jump(*o);
                    },
                }
                if self.iserr {
                    self.dump_stack();
                    break InterpretResult::InterpretCompileError; 
                }
            } else {
                break InterpretResult::InterpretOK;
            };
        }
    }
}

impl<const STACK: usize> Default for CrVm<STACK> {
    fn default() -> Self {
        Self::new()
    }
}

pub fn interpret<'a, const S: usize>(
    vm: &mut CrVm<S>,
    ts: Seq<TokSeqItem<'a>>,
) -> InterpretResult {
    // maybe i'll chain the Seqs ??
    let chunk: RefCell<CrChunk> = RefCell::new(CrChunk::new());
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
