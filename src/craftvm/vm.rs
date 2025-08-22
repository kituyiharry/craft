#![allow(static_mut_refs)]
use ocaml::{Seq};
use once_cell::unsync::Lazy;
use std::{
    array, cell::{Cell, RefCell}, collections::HashMap, ops::{Add, Div, Mul, Sub}, rc::Rc
};

use crate::craftvm::{chunk::CraftChunkIter, debug, value::{are_same_type, CrFunc, CrObjType, CrObjVal}};
use super::compiler::{compile, CraftParser, FuncType::FScript, TokSeqItem};
use super::{common::OpCode, value::CrValue};

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

type CrFunctData = *const CrFunc;
pub static mut FNALLOCA: Lazy<Vec<CrFunctData>> = Lazy::new(||{
    Vec::with_capacity(64)
});

// drops vector items
pub(crate) unsafe fn free() {
    CRALLOCA.iter().for_each(|(t, c, p)| {
        log::debug!("freeing at {:?}", *p);
        drop(Vec::from_raw_parts(*p as *mut u8, *t, *c));
    });
    CRALLOCA.truncate(0);
}

pub(crate) unsafe fn alloca(bytes: Vec<u8>) -> (usize, *const u8) {
    let tot = bytes.len();
    let cap = bytes.capacity();
    let ptr = Vec::leak(bytes).as_ptr();
    CRALLOCA.push((tot, cap, ptr));
    (CRALLOCA.len(), ptr)
}

pub(crate) unsafe fn falloca(func: Box<CrFunc>) -> (usize, *mut CrFunc) {
    let ptr = Box::leak(func);
    let idx = FNALLOCA.len();
    FNALLOCA.push(ptr);
    (idx, ptr)
}

const MAX_FRAMES: usize = 64;

// A CallFrame represents a single ongoing function call.
#[derive(Debug, Clone)]
pub struct CrCallFrame<'a> {
    pub fncobj: *mut CrFunc,
    pub instrp: *mut CraftChunkIter<'a>,
    pub prvfrm: usize, // where the frameptr previously was
    pub prvarg: usize, // the total argument count!
}

// objects are tracked in alloca and free
pub struct CrVm<'a, const STACKSIZE: usize> {
    // source: Rc<RefCell<CrFunc>>,
    // The value stack in the VM works on the observation that 
    // local variables and temporaries behave in a last-in first-out fashion.
    vstack: [Rc<Cell<CrValue>>; STACKSIZE],
    stkptr: *mut CrValue, // stack pointer
    stkidx: usize,
    global: HashMap<String, Rc<Cell<CrValue>>>,
    iserr : bool, // sentinel for breaking run loop from other methods ;-) - bad design lol
    frames: [Option<RefCell<CrCallFrame<'a>>>; MAX_FRAMES],
    frmcnt: usize,
    frmptr: usize,
    frmags: usize,
}

impl<'a, const S: usize> Drop for CrVm<'a, S> {
    fn drop(&mut self) {
        unsafe { 
            free(); 
        };
    }
}

#[allow(dead_code)]
impl<'a, const STACK: usize> CrVm<'a, STACK> {
    pub fn new() -> Self {
        log::debug!("Creating new VM");
        let vstck = array::from_fn::<_, STACK, _>(|_idx| Rc::new(Cell::new(CrValue::CrNil)));
        let frmes = array::from_fn::<_, MAX_FRAMES, _>(|_idx| None);
        let stcki = 0;
        let stckp = vstck[stcki].as_ptr();
        Self {
            //source: Rc::new(RefCell::new(CrFunc::default())),
            vstack: vstck,
            stkptr: stckp,
            stkidx: stcki,
            global: HashMap::new(),
            iserr:  false,
            frames: frmes,
            frmcnt: 0,
            frmptr: 0,
            frmags: 0,
        }
    }

    pub fn reset_stack(&mut self) {
        log::debug!("Stack Reset");
        self.stkidx = 0;
        self.stkptr = self.vstack[0].as_ptr();
        // make sure to reset once done with a frame!
        self.frmcnt = 0;
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
                log::info!("popped value {:?} at ({})", *(self.stkptr), self.stkidx+1);
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
                let v = f(*a, *b);
                log::debug!("f({}, {}) -> {}", *a, *b, v);
                self.push(v);
            } else {
                self.dump_stack();
                log::error!("Attempt binary op on mistyped values {} and  {}!", 
                    *a, *b
                );
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
            println!("  {pos} ==> {:?}", self.vstack[pos].clone());
            pos-=1;
        }
        println!("  {pos} ==> {:?}", self.vstack[pos].clone());
        println!("============= stack ================");
    }

    pub fn dump_globals(&self) {
        println!("============= globals ================");
        self.global.iter().for_each(|(k, v)| {
            println!("  {k} => {:?}", v.clone().get())
        });
        println!("============= globals ================");
    }

    pub fn dump_src(&self) {
        // This one drops it
        if self.frmcnt > 0 {
            unsafe { 
                let bsrc = (self.frames[self.frmcnt-1].as_ref().unwrap().borrow_mut()).fncobj;

                let name = &(*bsrc).fname;
                if name.is_empty() {
                    println!("============= script ===============");
                } else {
                    println!("============= {name} ===============");
                }

                let cchnk = &(*bsrc).chunk;
                debug::disas(cchnk);
                println!("====================================");
            };
        }
    }

    pub fn warm(&mut self, mut chunk: CrFunc) {
        chunk.fname = "script".into();
        self.reset_stack();

        let (_idx, gbox) = unsafe { falloca(Box::new(chunk)) };

        let v = CrObjVal {
            objtype: CrObjType::CrFunc,
            objlen:  0, // Not used?/
            objval:  gbox,
            next:    Some(_idx),
        };

        self.push(CrValue::CrObj(v));

        let iter = unsafe { (*gbox).chunk.into_iter() };
        let frame = CrCallFrame {
            fncobj: gbox,
            instrp: Box::leak(Box::new(iter)),
            prvfrm: 0, // a way to restore the frame pointer from calls
            prvarg: 0, // main script doesn't receive args - for now
        };
        self.frames[self.frmcnt].replace(frame.into());
        self.frmcnt = 1;
    }

    // Convenience function so all you have to do is manipulate the framecount

    #[inline]
    pub fn curframe(&self) -> *mut CrFunc {
        self.frame(self.frmcnt - 1)
    }

    #[inline]
    pub fn curinstrptr(&self) -> *mut CraftChunkIter<'a> {
        self.instrptr(self.frmcnt - 1)
    }


    #[inline]
    pub fn calleefrm(&self) -> usize {
        unsafe {
            (self.frames[self.frmcnt - 1]).as_ref().unwrap_unchecked().borrow().prvfrm
        }
    }

    #[inline]
    pub fn calleeargs(&self) -> usize {
        unsafe {
            (self.frames[self.frmcnt - 1]).as_ref().unwrap_unchecked().borrow().prvarg
        }
    }

    #[inline]
    pub fn frame(&self, idx: usize) -> *mut CrFunc {
        unsafe {
            (self.frames[idx]).as_ref().unwrap_unchecked().borrow().fncobj
        }
    }

    #[inline]
    pub fn instrptr(&self, idx: usize) -> *mut CraftChunkIter<'a> {
        unsafe {
            (self.frames[idx]).as_ref().unwrap_unchecked().borrow_mut().instrp
        }
    }


    // #[inline]
    // pub fn pushslot(&self, value: CrValue) {
    //     unsafe {
    //         (self.frames.last().unwrap_unchecked()).as_ref().unwrap_unchecked().borrow_mut().slots.push(value.into());
    //     }
    // }

    // #[inline]
    // pub fn popslot(&self, idx: usize) -> *mut CrValue {
    //     unsafe {
    //         let f = (self.frames.last().unwrap_unchecked()).as_ref().unwrap_unchecked().borrow_mut(); 
    //         log::debug!("getting slot {idx} of {}", f.slots.len());
    //         f.slots[idx].as_ptr()
    //     }
    // }

    // #[inline]
    // pub fn setslot(&self, idx: usize, value: CrValue) {
    //     unsafe {
    //         let mut f = (self.frames.last().unwrap_unchecked()).as_ref().unwrap_unchecked().borrow_mut(); 
    //         let l = f.slots.len();
    //         if idx >= l {
    //             log::debug!("pushing slot {idx} of {l} to {value}");
    //             f.slots.push(value.into());
    //         } else {
    //             log::debug!("modifying slot {idx} of {l} to {value}");
    //             f.slots[idx].set(value);
    //         }
    //     }
    // }
    fn print_stacktrace(&self) {
        println!("========== stacktrace ============");
        unsafe {
            (0..self.frmcnt).rev().for_each(|i| {
                let o = self.frames[i].as_ref().unwrap_unchecked().borrow();
                let f = &(*o.fncobj);
                if let Some((x, y, z)) = (*o.instrp).lastin {
                    println!("  <fn({},ar={})\t at [line:{y},offst{x}]::{z}>", f.fname, f.arity);
                }
            });
        }
        println!("==================================");
    }

    fn call_value(&mut self, argc: usize, val: *mut CrValue) -> bool {
        unsafe {
            match *val {
                CrValue::CrObj(obj) => {
                    if self.frmcnt >= MAX_FRAMES {
                        log::error!("Maximum call frames reached: {} of {}", self.frmcnt, MAX_FRAMES);
                        return false;
                    }
                    match obj.objtype {
                        CrObjType::CrFunc => {
                            let b = Box::from_raw(obj.objval as *mut CrFunc);
                            let p = Box::leak(b);
                            if argc != p.arity {
                                log::error!("Expected {} arity, got {argc}", p.arity);
                                return false;
                            }

                            // push new frame
                            log::info!("pushing new frame for {}", p.fname);
                            let frame = CrCallFrame {
                                fncobj: p,
                                instrp: Box::leak(Box::new(p.chunk.into_iter())),
                                prvfrm: self.frmptr,
                                prvarg: argc,
                            };
                            
                            // dear God ??
                            self.frmptr = self.stkidx - argc - (self.frmcnt + 1);

                            self.frames[self.frmcnt].replace(RefCell::new(frame));
                            self.frmcnt += 1;
                            true
                        }, 
                        _ => { 
                            self.dump_stack();
                            log::error!("Call Value must be a function object, found {obj:?}");
                            false
                        }
                    }
                },
                e => {
                    self.dump_stack();
                    log::error!("Call Value must be a function object, found {e:?}");
                    false
                }
            }
        }
    }

    pub fn run(&mut self) -> InterpretResult {
        log::debug!("== vm exec ==");
        self.dump_src();

        // At the beginning of each function call, the VM records the location of 
        // the first slot where that function’s own locals begin. 
        // The instructions for working with local variables access them by 
        // a slot index relative to that, instead of relative to the bottom of the stack initially
        //
        //The VM needs to return back to the chunk where the function was called 
        //from and resume execution at the instruction immediately after the call. 
        //Thus, for each function call, we need to track where we jump back to when the call completes.
        //This is called a return address because it’s the address of the instruction that the VM returns to after the call
        //
        // TODO

        // start vm thread
        unsafe {
        loop {
            if let Some((_idx, _line, op)) = (*self.curinstrptr()).next() {
                //self.dump_stack();
                if log::log_enabled!(log::Level::Debug) {
                    super::debug::disas_instr(&(*(self.curframe())).chunk, _idx, _line, op);
                }
                // TODO: in the book, they peek before executing
                // In the book some instructions are 'merged' as well so we have to 
                // look ahead in this version as well. Its best to just implement all instructions 
                // directly for simplicity for future reference i.e for >=, <= .. etc
                match op {
                    OpCode::OpNop => log::debug!("Nop instruction"),
                    OpCode::OpPop => { self.pop(); },
                    OpCode::OpReturn => { 
                            // likely nil
                            let v = self.pop();
                            // restore enclosing values
                            self.frmptr = self.calleefrm();
                            self.frmags = self.calleeargs();
                            if self.frmcnt == 1 {
                                // likely popping the func itself
                                (0..self.frmags).for_each(|_| { 
                                    self.pop();
                                });
                                return InterpretResult::InterpretOK;
                            }
                            self.frmcnt -= 1;
                            // pop the callstack arguments
                            (0..=self.frmags).for_each(|_| { 
                                let _ = self.pop();
                            });
                            self.push(*v);
                    }
                    OpCode::OpNegate => {
                                            let pop = self.pop();
                                            if matches!(*pop, CrValue::CrNumber(_)) {
                                                log::debug!("negate: {}!",*pop);
                                                self.push(-(*pop)) ;
                                            } else {
                                                self.push(*pop);
                                                log::error!("expected a number at stack top");
                                                self.dump_stack();
                                                return InterpretResult::InterpretCompileError;
                                            }
                                    }
                    OpCode::OpNot  => {
                                            let pop = self.pop();
                                            if matches!(*pop, CrValue::CrBool(_) | CrValue::CrNil) {
                                                log::debug!("invert: {}!",*pop);
                                                self.push(!(*pop)) ;
                                            } else {
                                                self.push(*pop);
                                                log::error!("expected a bool-like at stack top");
                                                self.dump_stack();
                                                return InterpretResult::InterpretCompileError;
                                            }
                                    },
                    OpCode::OpSub  => self.binop(CrValue::sub),
                    OpCode::OpAdd  => self.binop(CrValue::add),
                    OpCode::OpMult => self.binop(CrValue::mul),
                    OpCode::OpDiv  => self.binop(CrValue::div),
                    OpCode::OpCnst(idx) => self.push(*(*self.curframe()).chunk.fetch_const(*idx)),
                    OpCode::OpTrue  => self.push(CrValue::CrBool(true)),
                    OpCode::OpFalse => self.push(CrValue::CrBool(false)),
                    OpCode::OpNil   => self.push(CrValue::CrNil),
                    OpCode::OpEqual => {
                                        let a = self.pop();
                                        let b = self.pop();
                                            if are_same_type(*a, *b) {
                                                log::debug!("comparing {:?} == {:?}", *a, *b);
                                                self.push(CrValue::CrBool(*a == *b));
                                            } else {
                                                log::error!(
                                                    "attempt to compare non-matched types: {:?} and {:?}",
                                                    *a, *b
                                                );
                                                self.push(*b);
                                                self.push(*a);
                                                self.dump_stack();
                                                break InterpretResult::InterpretCompileError;
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
                    OpCode::OpPrint => println!("{:}", *self.pop()),
                    OpCode::OpDefGlob(idx) => {
                        // likely a string object
                        // This code doesn’t check to see if the key is already in the table. 
                        // Lox is pretty lax with global variables and lets you redefine them without error. 
                        // That’s useful in a REPL session, so the VM supports that 
                        // by simply overwriting the value if the key happens to already be in the hash table.
                        
                        // get the var name as string
                        let identobj = (*(self.curframe())).chunk.fetch_const(*idx).to_string();
                        let v = *self.pop();
                        log::debug!("glob defined: {identobj} => {v}");
                        self.global.insert(identobj, Rc::new(Cell::new(v))); 
                    },
                    OpCode::OpGetGlob(g) => {
                        match self.global.get(g) {
                            Some(v) => {
                                log::debug!("glob fetched: {g}");
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
                        // TODO: maybe use smolstr, kstring or raw_entry
                        match self.global.entry(s.clone()) {
                            std::collections::hash_map::Entry::Occupied(o) => {
                                    log::debug!("glob set: {s} => {}", *nv);
                                    o.get().set(*nv);
                            },
                            std::collections::hash_map::Entry::Vacant(_v) => {
                                log::error!("tried to set undefined variable {s}");
                                self.dump_stack();
                                break InterpretResult::InterpretCompileError
                            }
                        }
                    },
                    OpCode::OpGetLoc(s, n) => {
                        let b = self.vstack[self.frmptr + *n + (self.frmcnt - 1)].get();
                        self.push(b);
                    },
                    OpCode::OpSetLoc(s, n) => {
                        let v = self.pop();
                        self.vstack[self.frmptr + *n + (self.frmcnt - 1)].set(*v);
                    },
                    // unlike the book i don't pop the condition ??
                    // not sure the effect :-D
                    OpCode::OpJumpIfFalse(o) => {
                            let v = self.pop();
                            if let CrValue::CrBool(n) = *v {
                                if !n {
                                    log::debug!("jumping {}", *o);
                                    (*(self.curinstrptr())).jump(*o);
                                }
                            } else {
                                // all other values are falsey!!
                                log::debug!("jumping {}", *o);
                                (*(self.curinstrptr())).jump(*o);
                            }
                    },
                    OpCode::OpJump(o) => {
                        log::debug!("raw jumping {}", *o);
                        (*(self.curinstrptr())).jump(*o);
                    },
                    OpCode::OpLoop(loc) => {
                        log::debug!("to location {}", *loc);
                        (*(self.curinstrptr())).goto(*loc);
                    },
                    OpCode::OpCall(_s, c) => {
                        let v = self.vstack[self.stkidx - c - 1].as_ptr();
                        if !self.call_value(*c, v) {
                            self.print_stacktrace();
                            return InterpretResult::InterpretRuntimeError;
                        }
                    }
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
}

impl<'a, const STACK: usize> Default for CrVm<'a, STACK> {
    fn default() -> Self {
        Self::new()
    }
}

pub fn interpret<'a, const S: usize>(
    vm: &mut CrVm<S>,
    ts: Seq<TokSeqItem<'a>>,
) -> InterpretResult {
    // maybe i'll chain the Seqs ??
    let chunk: RefCell<CrFunc> = RefCell::new(CrFunc::default());  // main function
    let mut parser: CraftParser = CraftParser::new(ts, chunk, FScript);
    if !compile(&mut parser) {
        log::error!("Compilation Error");
        InterpretResult::InterpretCompileError
    } else {
        let chnk = parser.finish();
        log::info!("executing");
        vm.warm(chnk);
        //vm.warm(parser.chnk.into_inner().chunk);
        vm.run()
    }
}
