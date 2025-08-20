#![allow(static_mut_refs)]
use core::f64;
use std::{any::Any, collections::{hash_map::Entry, HashMap}, fmt::{Debug, Display}, hash::{Hash, Hasher}, ops::{Add, Div, Mul, Neg, Not, Sub}};

use crate::craftvm::{chunk::CrChunk, vm::{self, falloca}};

static MIN_VEC_CAP: usize = 8;

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub enum CrObjType {
    CrStr,
    CrFunc,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct CrObjVal {
    pub objtype: CrObjType,
    pub objval:  *const dyn Any, // :-(
    pub objlen:  usize,
    pub next:    Option<usize>
}

impl Hash for CrObjVal {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let as_str = unsafe {
            std::str::from_utf8_unchecked(
                std::slice::from_raw_parts(self.objval as *const u8, self.objlen)
            )
        };
        as_str.hash(state)
    }
}

impl Debug for CrObjVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.objtype {
            CrObjType::CrStr => {
                let as_str = unsafe {
                    std::str::from_utf8_unchecked(
                        std::slice::from_raw_parts(self.objval as *const u8, self.objlen)
                    )
                };
                write!(f, "{:?}{{{}@{:?}}}", self.objtype, as_str, self.objval)
            },
            CrObjType::CrFunc => {
                let fch = unsafe{ Box::from_raw(self.objval as *mut CrFunc) };
                let f = Debug::fmt(&fch, f);
                Box::leak(fch);
                f
            },
        }
    }
}

impl<'a> From<&'a str> for CrObjVal {
    fn from(value: &'a str) -> Self {
        CrObjVal {
            objtype: CrObjType::CrStr,
            objlen: std::mem::size_of_val(value),
            objval: value.as_ptr(),
            next:   None,
        }
    }
}

impl From<Box<CrFunc>> for CrObjVal {
    fn from(value: Box<CrFunc>) -> Self {
        let (_id, ptr) = unsafe {
            falloca(value)
        };
        CrObjVal {
            objtype: CrObjType::CrFunc,
            objlen:  0, // Not used?/
            objval:  ptr,
            next:    Some(_id),
        }
    }
}

impl PartialEq for CrObjVal {
    fn eq(&self, other: &Self) -> bool {
        match self.objtype  {
            CrObjType::CrFunc => {},
            CrObjType::CrStr => {
                if self.objlen == other.objlen {
                    unsafe {
                        let lhand: &str = {
                            std::str::from_utf8_unchecked(std::slice::from_raw_parts(self.objval as *const u8, self.objlen))
                        };
                        let rhand: &str = {
                            std::str::from_utf8_unchecked(std::slice::from_raw_parts(other.objval as *const u8, other.objlen))
                        };
                        return lhand.eq(rhand);
                    }
                }
            }
        }
        false
    }
}

impl Add for CrObjVal {
    type Output = CrObjVal;

    fn add(self, rhs: Self) -> Self::Output {
        match self.objtype {
            CrObjType::CrStr => {
                    let lhand: &[u8] = unsafe {
                        std::slice::from_raw_parts(self.objval as *const u8, self.objlen)
                    };
                    let rhand: &[u8] = unsafe {
                        std::slice::from_raw_parts(rhs.objval as *const u8, rhs.objlen)
                    };
                    let mut v: Vec<u8> = Vec::with_capacity(MIN_VEC_CAP);
                    v.extend_from_slice(lhand);
                    v.extend_from_slice(rhand);
                    let tot = v.len();
                    // leak to avoid being dropped while referenced
                    unsafe { 
                        let (_id, ptr) = vm::alloca(v);
                        CrObjVal {
                            objtype: CrObjType::CrStr,
                            objval:  ptr,
                            objlen:  tot,
                            next:    Some(_id),
                        }
                    }
            }, 
            _ => panic!("cannot add functions!")
        }
    }
}

impl Display for CrObjVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.objtype {
            CrObjType::CrFunc => {
                let b = unsafe {
                    Box::from_raw(self.objval as *mut CrFunc)
                };
                Debug::fmt(&*b, f)
            },
            CrObjType::CrStr => {
                let outword: &str = unsafe { 
                    std::str::from_utf8_unchecked(std::slice::from_raw_parts(self.objval as *const u8, self.objlen))
                };
                write!(f, "{outword}")
            },
        }
    }

}

#[derive(Debug, Clone, Copy)]
pub enum CrValue {
    CrNumber(f64),
    CrBool(bool),
    CrNil,
    CrObj(CrObjVal),
}

/// Returns the mantissa, exponent and sign as integers.
#[inline]
fn integer_decode(val: f64) -> (u64, i16, i8) {
    let bits: u64 = f64::to_bits(val);
    let sign: i8  = if bits >> 63 == 0 { 1 } else { -1 };
    let mut exponent: i16 = ((bits >> 52) & 0x7ff) as i16;
    let mantissa = if exponent == 0 {
        (bits & 0xfffffffffffff) << 1
    } else {
        (bits & 0xfffffffffffff) | 0x10000000000000
    };
    // Exponent bias + mantissa shift
    exponent -= 1023 + 52;
    (mantissa, exponent, sign)
}

impl Hash for CrValue  {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            CrValue::CrNumber(n) => {
                let (m, e, s) = integer_decode(*n);
                Hasher::write_i8( state, s);
                Hasher::write_i16(state, e);
                Hasher::write_u64(state, m);
            },
            CrValue::CrBool(b) => {
                Hasher::write_u8(state, if *b { 1u8 } else { 0u8 });
            },
            CrValue::CrNil => { },
            CrValue::CrObj(cr_obj_val) => {
                cr_obj_val.hash(state);
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct ConstPool {
    pub vals: Vec<CrValue>,
    pub intr: HashMap<String, usize>, // for string interning
}

impl Hash for ConstPool {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.vals.hash(state);
    }
}

impl ConstPool {
    pub fn new() -> Self {
        Self { vals: vec![], intr: HashMap::new() }
    }

    pub fn intern(&mut self, val: &str) -> usize {
        match self.intr.entry(val.to_owned()) {
            Entry::Vacant(v) => {
                let idx = self.vals.len();
                self.vals.push(CrValue::CrObj(CrObjVal::from(v.key().as_str())));
                v.insert(idx);
                idx
            },
            Entry::Occupied(o) => {
                *o.get()
            }
        }
    }

    pub fn insert(&mut self, val: CrValue) -> usize {
        let idx = self.vals.len();
        self.vals.push(val);
        idx
    }

    pub fn get(&self, idx: usize) -> &CrValue {
       unsafe { self.vals.get_unchecked(idx) }
    }
}

pub fn are_same_type(a: CrValue, b: CrValue) -> bool {
    matches!((a, b), 
        (CrValue::CrBool(_),   CrValue::CrBool(_))   | 
        (CrValue::CrNumber(_), CrValue::CrNumber(_)) | 
        (CrValue::CrNil,       CrValue::CrNil)       | 
        (CrValue::CrObj(_),    CrValue::CrObj(_))
    )
}

impl Sub for CrValue  {
    type Output=CrValue;
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (CrValue::CrNumber(l), CrValue::CrNumber(r)) => {
                CrValue::CrNumber(f64::sub(l, r))
            },
            _ => panic!("Invalid sub operands: {self:?} - {rhs:?}"),
        }
    }
}

impl Mul for CrValue  {
    type Output=CrValue;
    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (CrValue::CrNumber(l), CrValue::CrNumber(r)) => {
                CrValue::CrNumber(f64::mul(l, r))
            },
            _ => panic!("Invalid mult operands: {self:?} * {rhs:?}"),
        }
    }
}

impl Neg for CrValue  {
    type Output=CrValue;
    fn neg(self) -> Self::Output {
        match self {
            CrValue::CrNumber(l) => {
                CrValue::CrNumber(f64::neg(l))
            },
            _ => panic!("Invalid neg operands: -{self:?}"),
        }
    }
}

impl Add for CrValue  {
    type Output=CrValue;
    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (CrValue::CrNumber(l), CrValue::CrNumber(r)) => {
                CrValue::CrNumber(f64::add(l, r))
            },
            (CrValue::CrObj(l), CrValue::CrObj(r)) => {
                CrValue::CrObj(l.add(r))
            },
            _ => panic!("Invalid add operands: {self:?} + {rhs:?}"),
        }
    }
}

impl Div for CrValue  {
    type Output=CrValue;
    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (CrValue::CrNumber(l), CrValue::CrNumber(r)) => {
                CrValue::CrNumber(f64::div(l, r))
            },
            _ => panic!("Invalid div operands: {self:?} / {rhs:?}"),
        }
    }
}

impl Not for CrValue {
    type Output=CrValue;
    fn not(self) -> Self::Output {
        match self {
            CrValue::CrBool(l) => {
                CrValue::CrBool(!l)
            },
            CrValue::CrNil => {
                CrValue::CrBool(true)
            },
            _ => panic!("Invalid neg operands: -{self:?}"),
        }
    }
}

impl PartialEq for CrValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::CrNumber(l0), Self::CrNumber(r0)) => *l0 == *r0,
            (Self::CrBool(l0),   Self::CrBool(r0))   => *l0 == *r0,
            (Self::CrNil,        Self::CrNil)        => true,
            (Self::CrObj(a),     Self::CrObj(b))     => CrObjVal::eq(a, b),
            _ => todo!()
        }
    }
}

impl PartialOrd for CrValue  {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::CrNumber(l0), Self::CrNumber(r0)) =>  f64::partial_cmp(l0, r0),
            (Self::CrBool(l0),   Self::CrBool(r0))   =>  (*l0).partial_cmp(r0),
            (Self::CrNil,        Self::CrNil )       =>  Some(std::cmp::Ordering::Equal),
            _ => None,
        }
    }
}

impl Display for CrValue  {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CrValue::CrNumber(n) =>  write!(f, "{n}"),
            CrValue::CrBool(b)   =>  write!(f, "{b}"),
            CrValue::CrNil       =>  write!(f, "nil"),
            CrValue::CrObj(obj)  =>  write!(f, "{obj}", ),
        }
    }
}

impl Default for ConstPool {
    fn default() -> Self {
        ConstPool::new()
    }
}

#[derive(Clone)]
pub struct CrFunc {
    pub fname: String,
    pub arity: usize, 
    pub chunk: CrChunk,
}

impl Display for CrFunc  {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn {}>", self.fname)
    }
}

impl Debug for CrFunc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "@func:{} - {} ops {{", self.fname, self.chunk.instrlen())?;
        Debug::fmt(&self.chunk, f)?;
        write!(f, "}}")
    }
}

impl Default for CrFunc  {
    fn default() -> Self {
        Self { 
            arity: 0, 
            chunk: CrChunk::new(), 
            fname: "".into() 
        }
    }
}

