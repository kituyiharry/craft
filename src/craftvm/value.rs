//pub type CraftValue = f64;

//use std::cell::RefCell;

use std::{ops::{Add, Div, Mul, Neg, Not, Sub}};

#[derive(Debug, Clone, Copy)]
pub enum CraftValue {
    CrNumber(f64),
    CrBool(bool),
    CrNil,
}

pub struct ConstPool {
    vals: Vec<CraftValue>,
}

impl ConstPool {
    pub fn new() -> Self {
        Self { vals: vec![] }
    }

    pub fn insert(&mut self, val: CraftValue) -> usize {
        let idx = self.vals.len();
        self.vals.push(val);
        idx
    }

    pub fn get(&self, idx: usize) -> &CraftValue {
       unsafe { self.vals.get_unchecked(idx) }
    }
}

pub fn are_same_type(a: CraftValue, b: CraftValue) -> bool {
    matches!(
        (a, b), 
        (CraftValue::CrBool(_),   CraftValue::CrBool(_)) | 
        (CraftValue::CrNumber(_), CraftValue::CrNumber(_)) | 
        (CraftValue::CrNil,       CraftValue::CrNil)
    )
}

impl Sub for CraftValue  {
    type Output=CraftValue;
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (CraftValue::CrNumber(l), CraftValue::CrNumber(r)) => {
                CraftValue::CrNumber(f64::sub(l, r))
            },
            _ => panic!("Invalid sub operands: {self:?} - {rhs:?}"),
        }
    }
}

impl Mul for CraftValue  {
    type Output=CraftValue;
    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (CraftValue::CrNumber(l), CraftValue::CrNumber(r)) => {
                CraftValue::CrNumber(f64::mul(l, r))
            },
            _ => panic!("Invalid mult operands: {self:?} * {rhs:?}"),
        }
    }
}

impl Neg for CraftValue  {
    type Output=CraftValue;
    fn neg(self) -> Self::Output {
        match self {
            CraftValue::CrNumber(l) => {
                CraftValue::CrNumber(f64::neg(l))
            },
            _ => panic!("Invalid neg operands: -{self:?}"),
        }
    }
}

impl Add for CraftValue  {
    type Output=CraftValue;
    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (CraftValue::CrNumber(l), CraftValue::CrNumber(r)) => {
                CraftValue::CrNumber(f64::add(l, r))
            },
            _ => panic!("Invalid add operands: {self:?} + {rhs:?}"),
        }
    }
}

impl Div for CraftValue  {
    type Output=CraftValue;
    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (CraftValue::CrNumber(l), CraftValue::CrNumber(r)) => {
                CraftValue::CrNumber(f64::div(l, r))
            },
            _ => panic!("Invalid div operands: {self:?} / {rhs:?}"),
        }
    }
}

impl Not for CraftValue {
    type Output=CraftValue;
    fn not(self) -> Self::Output {
        match self {
            CraftValue::CrBool(l) => {
                CraftValue::CrBool(!l)
            },
            CraftValue::CrNil => {
                CraftValue::CrBool(true)
            },
            _ => panic!("Invalid neg operands: -{self:?}"),
        }
    }
}

impl PartialEq for CraftValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::CrNumber(l0), Self::CrNumber(r0)) => *l0 == *r0,
            (Self::CrBool(l0),   Self::CrBool(r0))   => *l0 == *r0,
            (Self::CrNil,        Self::CrNil)        => true,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl PartialOrd for CraftValue  {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::CrNumber(l0), Self::CrNumber(r0)) =>  f64::partial_cmp(l0, r0),
            (Self::CrBool(l0), Self::CrBool(r0))     =>  (*l0).partial_cmp(r0),
            (Self::CrNil, Self::CrNil)               =>  None,
            _ => None,
        }
    }
}

impl Default for ConstPool {
    fn default() -> Self {
        ConstPool::new()
    }
}
