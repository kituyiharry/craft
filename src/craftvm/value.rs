#![allow(static_mut_refs)]
use std::{collections::HashMap, fmt::{Debug, Display}, hash::Hash, ops::{Add, Div, Mul, Neg, Not, Sub}};
use crate::craftvm::vm;

static MIN_VEC_CAP: usize = 8;

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub enum CrObjType {
    CrBorStr,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct CrObjVal {
    pub objtype: CrObjType,
    pub objval:  *const u8,
    pub objlen:  usize,
    pub next:    Option<usize>
}

impl Hash for CrObjVal {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let as_str = unsafe {
            std::str::from_utf8_unchecked(
                std::slice::from_raw_parts(self.objval, self.objlen)
            )
        };
        as_str.hash(state)
    }
}

impl Debug for CrObjVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let as_str = unsafe {
            std::str::from_utf8_unchecked(
                std::slice::from_raw_parts(self.objval, self.objlen)
            )
        };
        write!(f, "{:?}{{{}@{:?}}}", self.objtype, as_str, self.objval)
    }
}

impl<'a> From<&'a str> for CrObjVal {
    fn from(value: &'a str) -> Self {
        CrObjVal {
            objtype: CrObjType::CrBorStr,
            objlen: std::mem::size_of_val(value),
            objval: value.as_ptr(),
            next:   None,
        }
    }
}

impl PartialEq for CrObjVal {
    fn eq(&self, other: &Self) -> bool {
        match self.objtype {
            CrObjType::CrBorStr => {
                if self.objlen == other.objlen {
                    unsafe {
                        let lhand: &str = {
                            std::str::from_utf8_unchecked(std::slice::from_raw_parts(self.objval, self.objlen))
                        };
                        let rhand: &str = {
                            std::str::from_utf8_unchecked(std::slice::from_raw_parts(other.objval, other.objlen))
                        };
                        return lhand.eq(rhand);
                    }
                }
                false
            }
        }
    }
}

impl Add for CrObjVal {
    type Output = CrObjVal;

    fn add(self, rhs: Self) -> Self::Output {
        match self.objtype {
            CrObjType::CrBorStr => {
                    let lhand: &[u8] = unsafe {
                        std::slice::from_raw_parts(self.objval, self.objlen)
                    };
                    let rhand: &[u8] = unsafe {
                        std::slice::from_raw_parts(rhs.objval, rhs.objlen)
                    };
                    let mut v: Vec<u8> = Vec::with_capacity(MIN_VEC_CAP);
                    v.extend_from_slice(lhand);
                    v.extend_from_slice(rhand);
                    let tot = v.len();
                    // leak to avoid being dropped while referenced
                    unsafe { 
                        let (_id, ptr) = vm::alloca(v);
                        CrObjVal {
                            objtype: CrObjType::CrBorStr,
                            objval:  ptr,
                            objlen:  tot,
                            next:    Some(_id),
                        }
                    }
            }
        }
    }
}

impl Display for CrObjVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.objtype {
            CrObjType::CrBorStr => {
                let outword: &str = unsafe { 
                    std::str::from_utf8_unchecked(std::slice::from_raw_parts(self.objval, self.objlen))
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

pub struct ConstPool {
    vals: Vec<CrValue>,
    intr: HashMap<String, usize>, // for string interning
}

impl ConstPool {
    pub fn new() -> Self {
        Self { vals: vec![], intr: HashMap::new() }
    }

    pub fn intern(&mut self, val: &str) -> usize {
        match self.intr.entry(val.to_owned()) {
            std::collections::hash_map::Entry::Vacant(v) => {
                let idx = self.vals.len();
                self.vals.push(CrValue::CrObj(CrObjVal::from(v.key().as_str())));
                v.insert(idx);
                idx
            },
            std::collections::hash_map::Entry::Occupied(o) => {
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
