#![allow(static_mut_refs)]

use super::{common, scanner::CrTokenType};
use crate::craftvm::{common::OpType, value::{CrFunc, CrObjVal, CrValue}};
use ocaml::Seq;
use once_cell::unsync::Lazy;
use std::{
    array, cell::{RefCell, UnsafeCell}, fmt::Debug
};

const MAX_FUNC_ARGS: usize = 255usize;

pub type TokSeqItem<'a> = (CrTokenType<'a>, usize, usize);

#[derive(Debug, Default)]
pub struct ParseState {
    pub had_err: bool,
    pub panic_md: bool,
}

#[derive(Debug, Clone)]
pub struct TokenData<'a> {
    pub line: usize,
    pub col: usize,
    pub token: CrTokenType<'a>,
}

impl Default for TokenData<'_> {
    fn default() -> Self {
        Self {
            line: 0,
            col: 0,
            token: CrTokenType::CrEof,
        }
    }
}

// NB: if previous is EoF, then current
pub struct CraftParser<'a> {
    tokseq:     Seq<(CrTokenType<'a>, usize, usize)>,
    tokcount:   usize,
    current:    RefCell<TokenData<'a>>,
    previous:   RefCell<TokenData<'a>>,
    pub state:  RefCell<ParseState>,
    pub chnk:   RefCell<CrFunc>,  // Root is main
    pub locs:   RefCell<CraftLocState<'a>>,
    pub cexp:   CrTokenType<'a>,
    pub offs:   usize,
}

#[derive(Debug)]
pub enum Precedence {
    PrecNone,
    PrecAssignment, // =
    PrecOr,         // or
    PrecAnd,        // and
    PrecEquality,   // == !=
    PrecComparison, // < > <= >=
    PrecTerm,       // + -
    PrecFactor,     // * /
    PrecUnary,      // ! -
    PrecCall,       // . ()
    PrecPrimary,
}

impl Precedence {
    // TODO: just implement Ord
    pub fn order(&self) -> usize {
        match self {
            Precedence::PrecNone => 0,
            Precedence::PrecAssignment => 1,
            Precedence::PrecOr => 2,
            Precedence::PrecAnd => 3,
            Precedence::PrecEquality => 4,
            Precedence::PrecComparison => 5,
            Precedence::PrecTerm => 6,
            Precedence::PrecFactor => 7,
            Precedence::PrecUnary => 8,
            Precedence::PrecCall => 9,
            Precedence::PrecPrimary => 10,
        }
    }

    pub fn next(&self) -> Self {
        match self {
            Precedence::PrecNone => Precedence::PrecAssignment,
            Precedence::PrecAssignment => Precedence::PrecOr,
            Precedence::PrecOr => Precedence::PrecAnd,
            Precedence::PrecAnd => Precedence::PrecEquality,
            Precedence::PrecEquality => Precedence::PrecComparison,
            Precedence::PrecComparison => Precedence::PrecTerm,
            Precedence::PrecTerm => Precedence::PrecFactor,
            Precedence::PrecFactor => Precedence::PrecUnary,
            Precedence::PrecUnary => Precedence::PrecCall,
            Precedence::PrecCall => Precedence::PrecPrimary,
            Precedence::PrecPrimary => Precedence::PrecNone,
        }
    }
}

type ParseRs = Result<(), String>;
type ParseFn = Box<dyn (FnMut(&mut CraftParser<'_>, bool) -> ParseRs) + Send + Sync>;

// distinction between top-level(script) and funcs
pub enum FuncType {
    FScript, 
    FFunc,
}

pub struct CraftParseRule {
    pub prefix: Option<ParseFn>,
    pub infix:  Option<ParseFn>,
    pub precdc: Precedence,
}

static MAX_LOCAL_COUNT: isize = 255;

#[derive(Debug)]
pub struct CraftLocal<'a> {
    pub name:  CrTokenType<'a>,
    pub depth: isize,
}

impl<'a> Default for CraftLocal<'a> {
    fn default() -> Self {
        Self { name: Default::default(), depth: -1 }
    }
}

pub struct CraftLocState<'a> {
    pub locals: [RefCell<CraftLocal<'a>>; MAX_LOCAL_COUNT as usize],
    pub lcount: isize, 
    pub scope_depth: usize,
    // When the compiler reaches a function declaration,
    // it needs to emit code into the function’s chunk when compiling its body. 
    // At the end of the function body, the compiler needs to return to the previous chunk it was working with
    pub funcs: Vec<CrFunc>,
    pub ftype: FuncType,
}

impl<'a> Default for CraftLocState<'a>  {
    fn default() -> Self {
        Self { 
            locals:     array::from_fn(|_idx| {
                RefCell::new(CraftLocal::default())
            }), 
            lcount:      0, 
            scope_depth: 0, 
            funcs:       vec![],
            ftype:       FuncType::FScript,
        }
    }
}

impl<'a> CraftLocState<'a> {
    pub fn level(t: FuncType) -> Self {
        Self { 
            ftype: t,
            ..Default::default()
        }
    }
}

// I was in a hurry :-(
// like binding power ??
//
// Aside from those, the rest of the table is full of NULL and PREC_NONE.
// Most of those empty cells are because there is no expression associated with those tokens.
// You can’t start an expression with, say, `else`, and `}` would make for a pretty confusing infix operator
//
#[allow(clippy::redundant_closure)]
static mut PARSERULES: Lazy<[UnsafeCell<CraftParseRule>; 42]> = Lazy::new(|| {
    [
        // CrTokenType::CrLeftParen => 0,
        UnsafeCell::new(CraftParseRule {
            prefix: Some(Box::new(|s, b| CraftParser::grouping(s, b))),
            infix:  Some(Box::new(|s, b| CraftParser::call(s, b))),
            precdc: Precedence::PrecCall,
        }),
        // CrTokenType::CrRightParen => 1,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix: None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrLeftBrace => 2,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix: None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrRightBrace => 3,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix: None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrComma => 4,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix: None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrDot => 5,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix: None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrMinus => 6,
        UnsafeCell::new(CraftParseRule {
            prefix: Some(Box::new(|s, b| CraftParser::unary(s, b))),
            infix:  Some(Box::new(|s, b| CraftParser::binary(s, b))),
            precdc: Precedence::PrecTerm,
        }),
        // CrTokenType::CrPlus => 7,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix: Some(Box::new(|s, b| CraftParser::binary(s, b))),
            precdc: Precedence::PrecTerm,
        }),
        // CrTokenType::CrSemicolon => 8,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix: None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrSlash => 9,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix: Some(Box::new(|s, b| CraftParser::binary(s, b))),
            precdc: Precedence::PrecFactor,
        }),
        // CrTokenType::CrStar => 10,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix: Some(Box::new(|s, b| CraftParser::binary(s, b))),
            precdc: Precedence::PrecFactor,
        }),
        // CrTokenType::CrBang => 11,
        UnsafeCell::new(CraftParseRule {
            prefix: Some(Box::new(|s, b| CraftParser::unary(s, b))),
            infix:  None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrBangEqual => 12,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix:  Some(Box::new(|s, b| CraftParser::binary(s, b))),
            precdc: Precedence::PrecEquality,
        }),
        // CrTokenType::CrEqual => 13,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix: None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrEqualEqual => 14,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix:  Some(Box::new(|s, b| CraftParser::binary(s, b))),
            precdc: Precedence::PrecEquality,
        }),
        // CrTokenType::CrGreater => 15,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix:  Some(Box::new(|s, b| CraftParser::binary(s, b))),
            precdc: Precedence::PrecComparison,
        }),
        // CrTokenType::CrGreaterEqual => 16,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix:  Some(Box::new(|s, b| CraftParser::binary(s, b))),
            precdc: Precedence::PrecComparison,
        }),
        // CrTokenType::CrLess => 17,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix:  Some(Box::new(|s, b| CraftParser::binary(s, b))),
            precdc: Precedence::PrecComparison,
        }),
        // CrTokenType::CrLessEqual => 18,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix:  Some(Box::new(|s, b| CraftParser::binary(s, b))),
            precdc: Precedence::PrecComparison,
        }),
        // CrTokenType::CrIdentifier(_) => 19,
        UnsafeCell::new(CraftParseRule {
            prefix: Some(Box::new(|s, b| CraftParser::variable(s, b))),
            infix: None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrString(_) => 20,
        UnsafeCell::new(CraftParseRule {
            prefix: Some(Box::new(|s, b| CraftParser::string(s, b))),
            infix:  None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrNumber(_) => 21,
        UnsafeCell::new(CraftParseRule {
            prefix: Some(Box::new(|s, b| CraftParser::number(s, b))),
            infix: None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrAnd => 22,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix:  Some(Box::new(|s, b| CraftParser::and_(s, b))),
            precdc: Precedence::PrecAnd,
        }),
        // CrTokenType::CrClass => 23,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix: None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrElse => 24,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix: None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrFalse => 25,
        UnsafeCell::new(CraftParseRule {
            prefix: Some(Box::new(|s, b| CraftParser::literal(s, b))),
            infix: None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrFor => 26,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix: None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrFun => 27,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix: None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrIf => 28,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix: None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrNil => 29,
        UnsafeCell::new(CraftParseRule {
            prefix: Some(Box::new(|s, b| CraftParser::literal(s, b))),
            infix:  None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrOr => 30,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix:  Some(Box::new(|s, b| CraftParser::or_(s, b))),
            precdc: Precedence::PrecOr,
        }),
        // CrTokenType::CrPrint => 31,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix: None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrReturn => 32,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix: None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrSuper => 33,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix: None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrThis => 34,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix: None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrTrue => 35,
        UnsafeCell::new(CraftParseRule {
            prefix: Some(Box::new(|s, b| CraftParser::literal(s, b))),
            infix: None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrVar => 36,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix: None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrWhile => 37,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix: None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrEof => 38,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix: None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrPrintln => 39,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix: None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrNonpert => 40,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix: None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrAtMemo => 41,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix: None,
            precdc: Precedence::PrecNone,
        }),
    ]
});

impl Debug for CraftParser<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CraftParser")
            .field("current", &self.current)
            .field("previous", &self.previous)
            .field("state", &self.state)
            .finish()
    }
}

const MIN_LOC_IDX: isize = 0isize;

impl<'a> CraftParser<'a> {
    // So the compiler creates function objects during compilation. Then, at runtime, they are simply invoked
    // The more fundamental problem, though, is recursion. 
    // With recursion, you can be “in” multiple calls to the same function at the same time. Each needs its own memory for its local variables.
    pub fn new(tokseq: Seq<(CrTokenType<'a>, usize, usize)>, chnk: RefCell<CrFunc>, lvl: FuncType) -> Self {

        // the compiler implicitly claims stack slot zero for the VM’s own internal use
        // and variable accounting
        let mut l = CraftLocState { lcount: MIN_LOC_IDX, ..CraftLocState::level(lvl) };
        l.locals[0].borrow_mut().depth = 0;
        l.locals[0].borrow_mut().name  = CrTokenType::CrNonpert; 
        l.lcount += 1;

        Self {
            tokseq,
            chnk,
            tokcount: 0,
            current:  RefCell::new(TokenData::default()),
            previous: RefCell::new(TokenData::default()),
            state:    RefCell::new(ParseState::default()),
            locs:     RefCell::new(l),
            cexp:     CrTokenType::CrEof,
            offs:     0,
        }
    }

    fn number(&mut self, _assgnprec: bool) -> ParseRs {
        log::debug!("accepted a number!");
        let tok = self.previous.borrow();
        match &tok.token {
            // no need to check for max int sinze we use usize for the indexes
            CrTokenType::CrNumber(ref value) => {
                log::debug!("was number {value}");
                self.chnk.borrow_mut().chunk.add_const(CrValue::CrNumber(*value), tok.line);
                Ok(())
            }
            t => {
                log::error!("expected a number!");
                Err(format!("Not a number in {t:?}"))
            }
        }
    }

    // This function compiles the right operand, much like how unary() compiles its
    // own trailing operand. Finally, it emits the bytecode instruction that
    // performs the binary operation.
    fn binary(&mut self, _assgnprec: bool) -> ParseRs {
        log::debug!("accepted a binary operator!");
        log::debug!(
            "state is prev {:?} and curr as {:?}",
            self.previous.borrow().token,
            self.current.borrow().token
        );
        let tok = &self.previous.borrow().token.clone();
        let line = self.previous.borrow().line;
        unsafe {
            let _rule = PARSERULES.get(tok.order()).unwrap().get();
            self.parse_precedence(&((*_rule).precdc.next()))?;
            log::debug!(
                "out of precedence finally with prev {:?} and curr as {:?}",
                self.previous.borrow().token,
                self.current.borrow().token
            );
        }
        let ch = &mut (self.chnk.borrow_mut().chunk);
        log::debug!("pushing op {tok:?}");
        match tok {
            CrTokenType::CrPlus => {
                ch.emit_byte(OpType::Simple(common::OpCode::OpAdd), line);
                Ok(())
            }
            CrTokenType::CrMinus => {
                ch.emit_byte(OpType::Simple(common::OpCode::OpSub), line);
                Ok(())
            }
            CrTokenType::CrStar => {
                ch.emit_byte(OpType::Simple(common::OpCode::OpMult), line);
                Ok(())
            }
            CrTokenType::CrSlash => {
                ch.emit_byte(OpType::Simple(common::OpCode::OpDiv), line);
                Ok(())
            }

            CrTokenType::CrBangEqual => {
                ch.emit_byte(OpType::Simple(common::OpCode::OpEqual), line);
                ch.emit_byte(OpType::Simple(common::OpCode::OpNot), line);
                Ok(())
            }
            CrTokenType::CrEqualEqual => {
                ch.emit_byte(OpType::Simple(common::OpCode::OpEqual), line);
                Ok(())
            }
            CrTokenType::CrGreater => {
                ch.emit_byte(OpType::Simple(common::OpCode::OpGreater), line);
                Ok(())
            }
            CrTokenType::CrGreaterEqual => {
                ch.emit_byte(OpType::Simple(common::OpCode::OpLess), line);
                ch.emit_byte(OpType::Simple(common::OpCode::OpNot), line);
                Ok(())
            }
            CrTokenType::CrLess=> {
                ch.emit_byte(OpType::Simple(common::OpCode::OpLess), line);
                Ok(())
            }
            CrTokenType::CrLessEqual => {
                ch.emit_byte(OpType::Simple(common::OpCode::OpGreater), line);
                ch.emit_byte(OpType::Simple(common::OpCode::OpNot), line);
                Ok(())
            }
            _ => {
                log::error!("escaping operator token {tok:?}!");
                Ok(())
            }
        }
    }

    fn resolve_local(&self, name: &CrTokenType) -> isize {
        let loci = self.locs.borrow();
        let tot = loci.lcount;
        for i in (MIN_LOC_IDX..=tot).rev() {
            let l = loci.locals[i as usize].borrow();
            log::debug!("local: {i} is {l:?}");
        }
        for i in (MIN_LOC_IDX..=tot).rev() {
            log::debug!("checking at local: {i} of {tot}");
            let l = loci.locals[i as usize].borrow();
            if l.name.eq(name) {
                log::debug!("found at local position: {i}");
                // sentinel for self-redeclaration
                if l.depth == -1 {
                    self.error_at(&self.previous.borrow(), "Can't read local variable in its own initializer.");
                }
                return i;
            }
        }
        -1
    }

    fn named_var(&mut self, _assgnprec: bool) -> ParseRs {

        let pret = self.previous.borrow().token.clone();
        let line = self.previous.borrow().line;
        let argl = self.resolve_local(&pret);
        log::debug!("local resolved finished with {argl}");
        match pret {
            // identifier_const name
            CrTokenType::CrIdentifier(s) => { 
                // We’ve messed up the precedence handling because variable() 
                // doesn’t take into account the precedence of the surrounding expression 
                // that contains the variable. If the variable happens to be the right-hand 
                // side of an infix operator, or the operand of a unary operator,
                // then that containing expression is too high precedence to permit the =.

                // To fix this, variable() should look for and consume the = only 
                // if it’s in the context of a low-precedence expression. 
                // The code that knows the current precedence is, logically enough, parsePrecedence(). 
                // The variable() function doesn’t need to know the actual level. 
                // It just cares that the precedence is low enough to allow assignment, 
                // so we pass that fact in as a Boolean.

                if _assgnprec && self.mtch(CrTokenType::CrEqual) {
                    log::debug!("accepted named_var with lower precedence to or= assignment");
                    let _ = self.expression();
                    let ch = &mut (self.chnk.borrow_mut().chunk);
                    if argl != -1 {
                        ch.emit_byte(OpType::Simple(common::OpCode::OpSetLoc(s.into(), argl as usize)), line);
                    } else {
                        ch.emit_byte(OpType::Simple(common::OpCode::OpSetGlob((*s).to_owned())), line);
                    }
                } else {
                    log::debug!("not a named_var assignment");
                    let ch = &mut (self.chnk.borrow_mut().chunk);
                    if argl  != -1 {
                        ch.emit_byte(OpType::Simple(common::OpCode::OpGetLoc(s.into(), argl as usize)), line);
                    } else {
                        ch.emit_byte(OpType::Simple(common::OpCode::OpGetGlob((*s).to_owned())), line);
                    }
                }
            }
            ref s => {
                log::error!("expected string literal {s:?}");
            }
        };
        Ok(())
    }

    // Since assignment is the lowest-precedence expression, the only time we allow an assignment 
    // is when parsing an assignment expression or top-level expression like in an expression statement. 
    fn variable(&mut self, _assgnprec: bool) -> ParseRs {
        self.named_var(_assgnprec)
    }

    fn string(&mut self, _assgnprec: bool) -> ParseRs {
        log::debug!("accepted string expr!");
        let prev = self.previous.borrow();
        let ch = &mut (self.chnk.borrow_mut().chunk);
        match &prev.token {
            CrTokenType::CrString(s) => { 
                ch.add_const(
                    CrValue::CrObj(CrObjVal::from(*s)),
                    prev.line
                ); 
            }
            s => {
                log::error!("expected string literal {s:?}");
            }
        };
        Ok(())
    }


    fn literal(&mut self, _assgnprec: bool) -> ParseRs {
        log::debug!("accepted literal expr!");
        let prev = self.previous.borrow();
        let ch = &mut self.chnk.borrow_mut().chunk;
        match &prev.token {
            CrTokenType::CrFalse => ch.emit_byte(OpType::Simple(common::OpCode::OpFalse), prev.line),
            CrTokenType::CrTrue  => ch.emit_byte(OpType::Simple(common::OpCode::OpTrue),  prev.line),
            CrTokenType::CrNil   => ch.emit_byte(OpType::Simple(common::OpCode::OpNil),   prev.line),
            s => log::error!("unhandled literal {s:?}"),
        }
        Ok(())
    }

    fn unary(&mut self, _assgnprec: bool) -> ParseRs {
        log::debug!("accepted unary expr!");
        let prevt = self.previous.borrow().token.clone();
        let line  = self.previous.borrow().line;
        self.parse_precedence(&Precedence::PrecUnary)?;
        let ch = &mut self.chnk.borrow_mut().chunk;
        match prevt {
            // we emit the bytecode to perform the negation.
            // It might seem a little weird to write the negate instruction after
            // its operand’s bytecode since the - appears on the left,
            // but think about it in terms of order of execution:
            // We evaluate the operand first which leaves its value on the stack.
            // Then we pop that value, negate it, and push the result.
            CrTokenType::CrMinus => {
                ch.emit_byte(OpType::Simple(common::OpCode::OpNegate), line);
                Ok(())
            }
            CrTokenType::CrBang => {
                ch.emit_byte(OpType::Simple(common::OpCode::OpNot), line);
                Ok(())
            }
            _ => {
                log::error!("escaping unary operator");
                Ok(())
            }
        }
    }

    fn parse_precedence(&mut self, _prec: &Precedence) -> ParseRs {
        log::debug!("in precedence for {_prec:?}!");
        self.advance();
        //let prefrule = get_parse_rule(self.previous.borrow().token.order());
        let binding = unsafe {
            PARSERULES
                .get_mut(self.previous.borrow().token.order())
                .unwrap()
        };
        let prefrule = binding.get_mut();
        match prefrule.prefix {
            Some(ref mut _preffn) => {
                log::debug!(
                    "applying prefix rule at prev: {:?} and  curr: {:?}",
                    self.previous.borrow().token,
                    self.current.borrow().token
                );
                let can_assign =_prec.order() <= Precedence::PrecAssignment.order();
                match _preffn(self, can_assign) {
                    Ok(n) => n,
                    Err(e) => println!("Error from parse rule somewhere: {e}!!"),
                };
                // Now we look for an infix parser for the next token.
                // If we find one, it means the prefix expression we already compiled might be an operand for it.
                // But only if the call to parsePrecedence() has a precedence that is low enough to permit that infix operator.
                let order = _prec.order();
                unsafe {
                    while order <= ((*PARSERULES.get(self.current.borrow().token.order()).unwrap().get()).precdc).order()
                    {
                        // we consume the operator and hand off control to the infix parser we found.
                        // It consumes whatever other tokens it needs (usually the right operand) and returns back to parsePrecedence().
                        // Then we loop back around and see if the next token is also a valid infix operator that can take the entire
                        // preceding expression as its operand.
                        self.advance();
                        //let nprefrule = &get_parse_rule(self.previous.borrow().token.order()).infix;
                        let binding = PARSERULES
                            .get_mut(self.previous.borrow().token.order())
                            .unwrap();
                        let nprefrule = binding.get_mut();
                        if let Some(ref mut _npreffn) = nprefrule.infix {
                            log::debug!(
                                "now with prev: {:?} and  curr: {:?}",
                                self.previous.borrow().token,
                                self.current.borrow().token
                            );
                            match _npreffn(self, can_assign) {
                                Ok(n) => n,
                                Err(e) => log::error!("Error from parse rule somewhere: {e}!!"),
                            };
                        } else {
                            log::error!("the infix rule was empty");
                        }
                    }
                }
                Ok(())
            }
            None => {
                log::error!("the prefix rule was empty, prev: {:?}, cur: {:?}", 
                    self.previous.borrow().token,
                    self.current.borrow().token
                );
                Ok(())
            }
        }
    }

    // should also be able to work for infinite loops ?? for(;;) { ... }
    fn for_statement(&mut self) -> ParseRs {

        // id of jump instruction
        let mut jidx: isize = -1;
        self.begin_scope();
        {

            self.consume(CrTokenType::CrLeftParen, "for loop start paren")?;
            // initializer - should also match closing semi colons
            {
                if self.mtch(CrTokenType::CrSemicolon) {
                    // no initializer
                } else if self.mtch(CrTokenType::CrVar) {
                    self.var_declaration()?;
                } else {
                    self.expr_statement()?;
                }
            }

            let loopline = self.previous.borrow().line;
            let mut loopstrt = self.chnk.borrow().chunk.instrlen();

            // condition
            {
                //self.consume(CrTokenType::CrSemicolon, "condition end colon")?;
                if !self.mtch(CrTokenType::CrSemicolon) {
                    // doesn't match the semicolon closing
                    self.expression()?;
                    self.consume(CrTokenType::CrSemicolon, "condition close semicolon")?;

                    self.chnk.borrow_mut().chunk.emit_byte(OpType::Jumper(
                        common::OpCode::OpJumpIfFalse(0)
                    ), self.previous.borrow().line);
                    jidx = (self.chnk.borrow().chunk.instrlen()) as isize;
                }
            }

            // increment
            // NB: It appears textually before the body, but executes after it
            {
                //  we’ll jump over the increment, run the body, 
                //  jump back up to the increment, run it, 
                //  and then go to the next iteration
                if !self.mtch(CrTokenType::CrRightParen) {

                    self.chnk.borrow_mut().chunk.emit_byte(
                        OpType::Jumper(common::OpCode::OpJump(1)),
                        self.previous.borrow().line
                    );

                    let incid = self.chnk.borrow().chunk.instrlen();

                    self.expression()?;


                    self.chnk.borrow_mut().chunk.emit_byte(
                        OpType::Jumper(common::OpCode::OpLoop(loopstrt)), 
                        loopline
                    );

                    let upinc = self.chnk.borrow().chunk.instrlen();

                    self.chnk.borrow_mut().chunk.mod_byte(incid - 1, |op| {
                        *op = OpType::Jumper(common::OpCode::OpJump(upinc - incid));
                    });
                    // change to loop to the beginning of the increment
                    loopstrt = incid;

                    self.consume(CrTokenType::CrRightParen, "increment closer")?;
                }
            }


            self.statement()?;

            // this loop jumps to the increment expression first, 
            self.chnk.borrow_mut().chunk.emit_byte(OpType::Jumper(common::OpCode::OpLoop(loopstrt)), loopline);

            let n = self.chnk.borrow_mut().chunk.instrlen();

            if jidx > -1 {
                self.chnk.borrow_mut().chunk.mod_byte((jidx-1) as usize, |op| {
                    *op = OpType::Jumper(common::OpCode::OpJumpIfFalse(n - (jidx as usize)))
                });
            }
        }

        self.end_scope();
        Ok(())
    }

    fn while_statement(&mut self) -> ParseRs {
        //let line = self.previous.borrow().line;
        //self.chnk.borrow_mut().emit_byte(OpType::Simple(common::OpCode::OpNop), line);
        let loopstrt = self.chnk.borrow().chunk.instrlen();
        self.consume(CrTokenType::CrLeftParen, "open while condition expected")?;
        self.expression()?;
        self.consume(CrTokenType::CrRightParen,"close while condition expected")?;
        let line = self.previous.borrow().line;

        self.chnk.borrow_mut().chunk.emit_byte(OpType::Jumper(common::OpCode::OpJumpIfFalse(0)), line);
        let offs = self.chnk.borrow().chunk.instrlen();
        self.statement()?;
        self.chnk.borrow_mut().chunk.emit_byte(OpType::Jumper(common::OpCode::OpLoop(loopstrt)), line);
        let upd = self.chnk.borrow().chunk.instrlen();

        self.chnk.borrow_mut().chunk.mod_byte(offs-1, |op| {
            *op = OpType::Jumper(common::OpCode::OpJumpIfFalse(upd-offs));
        });

        Ok(())
    }

    // If that value is falsey, then we know the entire and must be false, 
    // so we skip the right operand and leave the left-hand side value as the result of the entire expression.
    // Otherwise, we discard the left-hand value and evaluate the right operand which becomes the result of the whole and expression.
    fn and_(&mut self, _assgnprec: bool) -> ParseRs {
        let ln = self.previous.borrow().line;
        self.chnk.borrow_mut().chunk.emit_byte(
            OpType::Simple(common::OpCode::OpJumpIfFalse(0)), ln
        );

        let off = self.chnk.borrow().chunk.instrlen();
        self.parse_precedence(&Precedence::PrecAnd)?;
        let upd = self.chnk.borrow().chunk.instrlen();

        self.chnk.borrow_mut().chunk.mod_byte(off-1, |op|{
            *op = OpType::Simple(common::OpCode::OpJumpIfFalse(upd-off))
        });

        Ok(())
    }

    // if the left-hand side is truthy, then we skip over the right operand. 
    // Thus we need to jump when a value is truthy. We could add a separate instruction, 
    // but just to show how our compiler is free to map the language’s semantics to whatever 
    // instruction sequence it wants, I implemented it in terms of the jump instructions we already have.
    //
    // If I’m honest with you, this isn’t the best way to do this. There are more instructions to dispatch and more overhead.
    fn or_(&mut self, _assgnprec: bool) -> ParseRs {
        let ln = self.previous.borrow().line;
        // infix check if top of stack is false, in order to skip the next jump instruction
        self.chnk.borrow_mut().chunk.emit_byte(
            OpType::Simple(common::OpCode::OpJumpIfFalse(1)), ln
        );
        self.chnk.borrow_mut().chunk.emit_byte(
            OpType::Simple(common::OpCode::OpJump(0)), ln
        );

        let jmp = self.chnk.borrow().chunk.instrlen();
        self.parse_precedence(&Precedence::PrecOr)?;
        let upd = self.chnk.borrow().chunk.instrlen();

        // modify jump to skip all remaining if the previous stack value was true
        // achieving short circuit
        self.chnk.borrow_mut().chunk.mod_byte(jmp-1, |op|{
            *op = OpType::Simple(common::OpCode::OpJump(upd-jmp))
        });

        Ok(())
    }

    fn grouping(&mut self, _assgnprec: bool) -> ParseRs {
        log::debug!("accepted grouping!");
        self.expression()?;
        self.consume(CrTokenType::CrRightParen, "Close grouping")
    }

    fn arg_list(&mut self) -> Result<usize, String>{
        let mut arg_count = 0usize;
        while !self.check(CrTokenType::CrRightParen) {
            self.expression()?;
            arg_count += 1;
            if !self.mtch(CrTokenType::CrComma) {
                break;
            }
        }
        self.consume(CrTokenType::CrRightParen, "argument list")?;
        log::debug!("found {arg_count} args");
        Ok(arg_count)
    }

    fn call(&mut self, _assgnprec: bool) -> ParseRs {
        log::debug!("accepted call expression, prev: {:?} cur: {:?}", self.previous.borrow().token, self.current.borrow().token);
        log::debug!("last identifier name could be {:?}", self.cexp);
        let mut fname = "anon";
        if let CrTokenType::CrIdentifier(n) = self.cexp {
            log::info!("setting called function name to {n}");
            fname = n;
        }
        let lino = self.previous.borrow().line;
        let argc = self.arg_list()?;
        self.chnk.borrow_mut().chunk.emit_byte(OpType::Simple(common::OpCode::OpCall(fname.into(), argc)), lino);
        Ok(())
    }

    fn expression(&mut self) -> ParseRs {
        log::debug!("accepted raw expression!");
        self.parse_precedence(&Precedence::PrecAssignment)?;
        Ok(())
    }

    // in case of an error, advance to a nearby checkpoint where we can resume parsing after the
    // error
    fn synchronize(&mut self) {
        // We skip tokens indiscriminately until we reach something that looks
        // like a statement boundary. We recognize the boundary by looking for a 
        // preceding token that can end a statement, like a semicolon. Or we’ll 
        // look for a subsequent token that begins a statement, usually one of 
        // the control flow or declaration keywords.
        self.state.borrow_mut().panic_md = false;

        while self.current.borrow().token != CrTokenType::CrEof {
            if self.previous.borrow().token == CrTokenType::CrSemicolon {
                return;
            }
            if matches!(
                self.current.borrow().token, 
                CrTokenType::CrClass | CrTokenType::CrFun   |
                CrTokenType::CrVar   | CrTokenType::CrFor   |
                CrTokenType::CrIf    | CrTokenType::CrWhile |
                CrTokenType::CrPrint | CrTokenType::CrReturn
            ) {
                return;
            }
            self.advance();
        }
    }

    fn print_statement(&mut self) -> ParseRs {
        log::debug!("accepted print statement");
        self.expression()?;
        let _ = self.consume_silent(CrTokenType::CrSemicolon, "expected a semicolon");
        let n = self.previous.borrow().line;
        self.chnk.borrow_mut().chunk.emit_byte(OpType::Simple(common::OpCode::OpPrint), n);
        Ok(())
    }

    fn expr_statement(&mut self) -> ParseRs {
        self.expression()?;
        let _ = self.consume_silent(CrTokenType::CrSemicolon, "Expect ';' after expression.");
        // Semantically, an expression statement evaluates the expression and discards the result. The compiler directly encodes that behavior. It compiles the expression, and then emits an OP_POP instruction
        self.chnk.borrow_mut().chunk.emit_byte(OpType::Simple(common::OpCode::OpPop), self.previous.borrow().line);
        Ok(())
    }

    fn begin_scope(&mut self) {
        self.locs.borrow_mut().scope_depth += 1
    }

    fn block(&mut self) -> ParseRs {
        while !self.check(CrTokenType::CrRightBrace) && !self.check(CrTokenType::CrEof) {
           self.declaration()?;
        }
        self.consume(CrTokenType::CrRightBrace, "expect closing brace")
    }

    fn end_scope(&mut self) {
        let mut loci = self.locs.borrow_mut(); 
        let mut ch = self.chnk.borrow_mut();
        loci.scope_depth -= 1;
        let ln = self.previous.borrow().line;
        while loci.lcount > 0 && (loci.locals[(loci.lcount as usize) - 1].borrow().depth as usize) > (loci.scope_depth)  {
            ch.chunk.emit_byte(OpType::Simple(common::OpCode::OpPop), ln);
            loci.lcount -= 1;
        }
    }

    // we use a classic trick called backpatching. We emit the jump instruction first with a placeholder offset operand. We keep track of where that half-finished instruction is. Next, we compile the then body. Once that’s done, we know how far to jump. So we go back and replace that placeholder offset with the real one now that we can calculate it. Sort of like sewing a patch onto the existing fabric of the compiled code.
    fn if_statement(&mut self) -> ParseRs {
        let ln = self.previous.borrow().line;
        self.consume(CrTokenType::CrLeftParen, "expected if expr in parens")?;
        self.expression()?;
        self.consume(CrTokenType::CrRightParen, "expected closing paren")?;

        let curoff = self.chnk.borrow().chunk.instrlen();
        let offset = 0usize;

        self.chnk.borrow_mut().chunk.emit_byte(
            OpType::Jumper(common::OpCode::OpJumpIfFalse(offset)), 
            ln
        );
        // capture position
        let idx = self.chnk.borrow().chunk.instrlen() - 1;

        self.statement()?;

        let updoff = self.chnk.borrow().chunk.instrlen();
        let offptr = updoff - curoff;

        self.chnk.borrow_mut().chunk.mod_byte(idx, |op| {
            *op = OpType::Jumper(common::OpCode::OpJumpIfFalse(offptr));
        });

        let elsoff = 0usize;
        self.chnk.borrow_mut().chunk.emit_byte(
            OpType::Jumper(common::OpCode::OpJump(elsoff)), 
            ln
        );
        
        // TODO: handle else statement
        if self.mtch(CrTokenType::CrElse) {
            let elsoff = self.chnk.borrow().chunk.instrlen();
            let idx2 = elsoff - 1;
            self.statement()?;
            let updoff = self.chnk.borrow().chunk.instrlen();
            let elsptr = updoff - elsoff;
            self.chnk.borrow_mut().chunk.mod_byte(idx2, |op| {
                *op = OpType::Jumper(common::OpCode::OpJump(elsptr));
            });
        }

        // Cell doesn't impl Drop so no need to std::mem::forget ???
        // std::mem::forget(offset);
        // std::mem::forget(elsoff);

        Ok(())
    }

    fn return_statement(&mut self) -> ParseRs { 
        // in the book they don't allow return from top-level script; i do;
        let l = self.previous.borrow().line;
        if self.mtch(CrTokenType::CrSemicolon) {
            // implicit nil
            self.end();
            Ok(())
        } else {
            self.expression()?;
            self.consume(CrTokenType::CrSemicolon, "return statement semi colon")?;
            self.chnk.borrow_mut().chunk.emit_byte(
                OpType::Simple(common::OpCode::OpReturn), 
                l
            );
            Ok(())
        }
    }

    fn statement(&mut self) -> ParseRs {
        log::debug!("accepted statement!");
        if self.mtch(CrTokenType::CrPrint) || self.mtch(CrTokenType::CrPrintln) {
           self.print_statement()
        } else if self.mtch(CrTokenType::CrIf) {
            self.if_statement()
        } else if self.mtch(CrTokenType::CrReturn) {
            self.return_statement()
        } else if self.mtch(CrTokenType::CrWhile) {
            self.while_statement()
        } else if self.mtch(CrTokenType::CrFor) {
            self.for_statement()
        } else if self.mtch(CrTokenType::CrLeftBrace) {
            self.begin_scope();
            let b = self.block();
            self.end_scope();
            b
        } else {
            self.expr_statement()
        }
    }

    fn add_local(&mut self, tok: CrTokenType<'a>) {
        log::debug!("adding local {tok:?}");
        if self.locs.borrow().lcount == MAX_LOCAL_COUNT {
            self.error_at(&self.previous.borrow(), "Too Many local vars!!");
            return;
        }
        let c = self.locs.borrow().lcount;
        self.locs.borrow_mut().lcount += 1;
        let binding = self.locs.borrow();
        let mut l = binding.locals[c as usize].borrow_mut(); 
        l.name = tok; 
        //l.depth = self.locs.borrow().scope_depth as isize;
        l.depth = -1;
    }

    // declareVariable
    fn decl_var(&mut self) -> ParseRs {
        let scpd = self.locs.borrow().scope_depth;
        if  scpd == 0 {
            return Ok(());
        }
        let lcnt = self.locs.borrow().lcount;
        let tok = self.previous.borrow().token.clone();
        for i in (MIN_LOC_IDX..(lcnt)).rev() {
            let l1 = &self.locs.borrow().locals[i as usize]; 
            let l = l1.borrow();
            log::debug!("check {l:?} for redeclarations");
            if l.depth != -1 && l.depth < (scpd as isize) {
                log::debug!("no redeclarations variables");
                break;
            }
            if l.name.eq(&tok) {
                log::error!("possible var redeclaration of {tok:?}");
                self.error_at(&self.previous.borrow(), "");
                return Err("Var redeclaration!".into());
            }
        }
        self.add_local(tok);
        Ok(())
    }

    fn parse_var(&mut self, message: &'static str) -> Result<usize, String> {
        self.consume(CrTokenType::CrIdentifier(""), message)?;

        self.decl_var()?;
        if self.locs.borrow().scope_depth > 0 {
            return  Ok(0);
        }

        let prev = self.previous.borrow();
        match prev.token {
            CrTokenType::CrIdentifier(ident) => {
                // return index of the var in the const pool
                // we don't add an instruction after like in add_const - check what comes after in
                // var_decl function
                Ok(self.chnk.borrow_mut().chunk.add_obj(
                    ident, 
                    //prev.line
                ))
            },
            _ => {
                Err("Expected Ident token in parse_var".to_owned())
            }
        }
    }

    fn mark_init(&mut self) {
        let loci = self.locs.borrow(); 
        if loci.scope_depth == 0 {
            return;
        }
        loci.locals[(loci.lcount - 1) as usize].borrow_mut().depth = loci.scope_depth as isize;
    }

    fn define_variable(&mut self, line: usize, cidx: usize) -> ParseRs {
        log::debug!("def var at idx: {cidx}");
        // defineVariable
        if self.locs.borrow().scope_depth > 0 {
            // markInitialized
            self.mark_init();
            log::debug!("mark init and exit");
            return Ok(());
        }
        log::debug!("defining global");
        self.chnk.borrow_mut().chunk.emit_byte(
            OpType::Simple(common::OpCode::OpDefGlob(cidx)), 
            line
        );
        Ok(())
    }

    fn var_declaration(&mut self) -> ParseRs {
        let global = self.parse_var("Expect var decl")?;

        let line   = self.previous.borrow().line;
        if self.mtch(CrTokenType::CrEqual) {
            let _ = self.expression();
        } else {
            let p = self.previous.borrow();
            let chnk = &mut self.chnk.borrow_mut().chunk;
            chnk.emit_byte(
                OpType::Simple(common::OpCode::OpNil), 
                p.line
            );
        }
        let _ = self.consume_silent(CrTokenType::CrSemicolon, "optional closer"); 
        self.define_variable(line, global)
    }


    fn function(&mut self, _ft: FuncType) -> ParseRs {
        let chunk: RefCell<CrFunc> = RefCell::new(CrFunc::default());  // main function
        let line = self.previous.borrow().line;
 
        // TODO: behaviour of clone here ??
        log::debug!("created a new parser");
        let mut parser = CraftParser::new(self.tokseq.clone(), chunk, _ft);
        parser.offs = self.offs + 1;

        if let CrTokenType::CrIdentifier(n) = self.previous.borrow().token {
            log::info!("setting function {n}");
            parser.chnk.borrow_mut().fname = n.into();
        }

        // start where we left
        parser.current  = self.current.borrow().clone().into();

        parser.begin_scope();

        parser.consume(CrTokenType::CrLeftParen,  "opening function parameters")?;
        while !parser.check(CrTokenType::CrRightParen) {
            if parser.chnk.borrow().arity > MAX_FUNC_ARGS {
                return Err("Max func args exceeded!".into());
            }
            let line = parser.current.borrow().line;
            let varidx = parser.parse_var("expected variable identifier")?;
            parser.define_variable(line, varidx)?;
            parser.chnk.borrow_mut().arity += 1;
            if !parser.mtch(CrTokenType::CrComma) {
                log::debug!("breaking from non=comma");
                break;
            }
        }
        parser.consume(CrTokenType::CrRightParen, "close function parameters")?;
        parser.consume(CrTokenType::CrLeftBrace,  "function body open")?;

        log::debug!("function block parsing start");
        parser.block()?;
        log::debug!("function block done, consumed {} tokens", parser.tokcount);


        // synchronize our state with the enclosed parser
        (0..=parser.tokcount).for_each(|_| { self.advance(); });
        if self.mtch(CrTokenType::CrRightBrace) && !self.advance() {
            self.current.replace(TokenData { 
                line: 0, col: 0, token: CrTokenType::CrEof 
            });
            log::debug!("end of advance");
        }

        let fbody = parser.finish();

        self.chnk.borrow_mut().chunk.add_const(
            CrValue::CrObj(CrObjVal::from(Box::new(fbody))),
            line
        );

        Ok(())
    }

    fn fun_declaration(&mut self) -> ParseRs {
        log::debug!("accepted function declaration");
        // write into the const pool
        let line = self.previous.borrow().line;
        let fnameidx = self.parse_var("function name expected?")?;
        //  we mark the function declaration’s variable “initialized” as soon as we compile the name, 
        //  before we compile the body. That way the name can be referenced inside the body without generating an error
        //  .. allows recursion
        self.mark_init();
        // generates code that leaves the resulting function object on top of the stack. 
        // After that, we call defineVariable() to store that function back into the variable we declared for it.
        log::debug!("begin function parsing for func at {fnameidx}");
        self.function(FuncType::FFunc)?;
        self.define_variable(line, fnameidx)
    }

    fn declaration(&mut self) -> ParseRs {
        log::debug!("accepted declaration!");
        if self.mtch(CrTokenType::CrSemicolon) && !self.advance() {
            self.advance();
           return Ok(()) 
        }
        if self.mtch(CrTokenType::CrFun) {
            return self.fun_declaration();
        }
        if self.mtch(CrTokenType::CrVar) {
            let _ = self.var_declaration();
            if self.state.borrow().panic_md {
                self.synchronize();
            }
            return Ok(());
        }
        let _ = self.statement(); 
        if self.state.borrow().panic_md {
            self.synchronize();
        }
        Ok(())
    }

    pub fn check(&self, ttype: CrTokenType) -> bool {
        // compare ordering
        log::debug!("check {:?} == {ttype:?}(expected)", self.current.borrow().token);
        self.current.borrow().token.order().eq(&ttype.order())
    }

    pub fn mtch(&mut self, ttype: CrTokenType) -> bool {
        if !self.check(ttype) {
            return false;
        }
        self.advance();
        true
    }

    pub fn advance(&mut self) -> bool {
        log::debug!("advancing!!");
        self.previous.swap(&self.current);
        // unlike the book impl, we are re-using the scanner in ocaml so we don't handle special
        // tokens here!
        if let Some(tok) = self.tokseq.next() {
            match tok {
                Ok((token, line, col)) => {
                    // cache last identifier
                    if matches!(token, CrTokenType::CrIdentifier(_)) {
                        self.cexp = token.clone()
                    }
                    self.tokcount += 1;
                    log::debug!("advance popped token {token:?}");
                    self.current.replace(TokenData { line, col, token });
                    return true;
                }
                Err(e) => {
                    log::error!("Token Sequence error: {e:?}!");
                    self.error_at(&self.previous.borrow(), "TokenSequence Error");
                    return false;
                }
            }
        };
        false
    }

    pub fn consume_silent(&mut self, expected: CrTokenType, message: &'static str) -> ParseRs {
        log::debug!("silently consuming a token, expecting {expected:?}!");
        if self.current.get_mut().token.order().eq(&expected.order()) {
            self.advance();
            return Ok(());
        }
        log::warn!("expected {expected:?} -> {message}");
        //self.error_at_curr(message);
        Ok(())
    }

    pub fn consume(&mut self, expected: CrTokenType, message: &'static str) -> ParseRs {
        log::debug!("consuming a token, prev: {:?} and cur {:?}, expecting {expected:?}!", self.previous.borrow().token, self.current.borrow().token);
        if self.current.get_mut().token.order().eq(&expected.order()) {
            self.advance();
            return Ok(());
        }
        self.error_at_curr(message);
        Err(message.into())
    }

    pub fn end(&mut self) {
        log::debug!("pushing return operation");
        self.chnk
            .get_mut()
            .chunk
            .end_compiler(self.current.borrow().line + 1);
    }

    // return the main function
    pub fn finish(mut self) -> CrFunc {
        log::debug!("finishing compilation");
        self.end();
        self.chnk.into_inner()
    }

    pub fn error_at(&self, tok: &TokenData, message: &'static str) {
        let mut state = self.state.borrow_mut();
        let line = tok.line;
        let coln = tok.col;
        let tokn = &tok.token;
        if state.panic_md {
            log::error!("already in panic mode!!");
            return;
        }

        if log::log_enabled!(log::Level::Debug) {
            let ch = &self.chnk.borrow().chunk;
            super::debug::disas(ch);
        }

        state.panic_md = true;
        log::error!("Error at line ({line}, {coln}), tok: {tokn:?}, {message} ");
        if let CrTokenType::CrEof = tokn {
            log::info!("End of File!");
            return;
        };
        state.had_err = true;
    }

    pub fn error_at_prev(&self, message: &'static str) {
        let curtok = &self.previous.borrow();
        self.error_at(curtok, message);
    }

    pub fn error_at_curr(&self, message: &'static str) {
        let curtok = &self.current.borrow();
        self.error_at(curtok, message);
    }
}

pub fn compile(_parser: &'_ mut CraftParser) -> bool {
    log::info!("compiling");
    _parser.advance();
    while !_parser.mtch(CrTokenType::CrEof) {
        _parser.declaration().unwrap();
        log::info!("compiler advancing expression");
    }
    //_parser.consume(CrTokenType::CrEof, "expected End of File").unwrap();
    //_parser.end();
    !_parser.state.borrow().had_err
}
