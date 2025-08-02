#![allow(static_mut_refs)]

use super::{chunk::CraftChunk, common, scanner::CrTokenType};
use crate::craftvm::common::OpType;
use ocaml::Seq;
use std::{cell::{RefCell, UnsafeCell}, fmt::Debug};
use once_cell::sync::Lazy;

pub type TokSeqItem<'a> = (CrTokenType<'a>, usize, usize);

#[derive(Debug, Default)]
pub struct ParseState {
    had_err: bool,
    panic_md: bool,
}

#[derive(Debug)]
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
    tokseq: Seq<(CrTokenType<'a>, usize, usize)>,
    current: RefCell<TokenData<'a>>,
    previous: RefCell<TokenData<'a>>,
    state: RefCell<ParseState>,
    pub chnk: RefCell<CraftChunk>,
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
type ParseFn = Box<dyn (FnMut(&mut CraftParser<'_>) -> ParseRs) + Send + Sync>;

pub struct CraftParseRule {
    pub prefix: Option<ParseFn>,
    pub infix:  Option<ParseFn>,
    pub precdc: Precedence,
}

// I was in a hurry :-(
// like binding power ??
//
// Aside from those, the rest of the table is full of NULL and PREC_NONE.
// Most of those empty cells are because there is no expression associated with those tokens.
// You can’t start an expression with, say, else, and } would make for a pretty confusing infix operator
//
#[allow(clippy::redundant_closure)]
static mut PARSERULES: Lazy<[UnsafeCell<CraftParseRule>; 42]> = Lazy::new(|| 
    [
    // CrTokenType::CrLeftParen => 0,
    UnsafeCell::new(CraftParseRule {
        prefix: Some(Box::new(|s| CraftParser::grouping(s))),
        infix: None,
        precdc: Precedence::PrecNone,
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
        prefix: Some(Box::new(|s| CraftParser::unary(s))),
        infix:  Some(Box::new(|s| CraftParser::binary(s))),
        precdc: Precedence::PrecTerm,
    }),
    // CrTokenType::CrPlus => 7,
    UnsafeCell::new(CraftParseRule {
        prefix: None,
        infix:  Some(Box::new(|s| CraftParser::binary(s))),
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
        infix:  Some(Box::new(|s| CraftParser::binary(s))),
        precdc: Precedence::PrecFactor,
    }),
    // CrTokenType::CrStar => 10,
    UnsafeCell::new(CraftParseRule {
        prefix: None,
        infix: Some(Box::new(|s| CraftParser::binary(s))),
        precdc: Precedence::PrecFactor,
    }),
    // CrTokenType::CrBang => 11,
    UnsafeCell::new(CraftParseRule {
        prefix: None,
        infix: None,
        precdc: Precedence::PrecNone,
    }),
    // CrTokenType::CrBangEqual => 12,
    UnsafeCell::new(CraftParseRule {
        prefix: None,
        infix: None,
        precdc: Precedence::PrecNone,
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
        infix: None,
        precdc: Precedence::PrecNone,
    }),
    // CrTokenType::CrGreater => 15,
    UnsafeCell::new(CraftParseRule {
        prefix: None,
        infix: None,
        precdc: Precedence::PrecNone,
    }),
    // CrTokenType::CrGreaterEqual => 16,
    UnsafeCell::new(CraftParseRule {
        prefix: None,
        infix: None,
        precdc: Precedence::PrecNone,
    }),
    // CrTokenType::CrLess => 17,
    UnsafeCell::new(CraftParseRule {
        prefix: None,
        infix: None,
        precdc: Precedence::PrecNone,
    }),
    // CrTokenType::CrLessEqual => 18,
    UnsafeCell::new(CraftParseRule {
        prefix: None,
        infix: None,
        precdc: Precedence::PrecNone,
    }),
    // CrTokenType::CrIdentifier(_) => 19,
    UnsafeCell::new(CraftParseRule {
        prefix: None,
        infix: None,
        precdc: Precedence::PrecNone,
    }),
    // CrTokenType::CrString(_) => 20,
    UnsafeCell::new(CraftParseRule {
        prefix: None,
        infix: None,
        precdc: Precedence::PrecNone,
    }),
    // CrTokenType::CrNumber(_) => 21,
    UnsafeCell::new(CraftParseRule {
        prefix: Some(Box::new(|s| CraftParser::number(s))),
        infix: None,
        precdc: Precedence::PrecNone,
    }),
    // CrTokenType::CrAnd => 22,
    UnsafeCell::new(CraftParseRule {
        prefix: None,
        infix: None,
        precdc: Precedence::PrecNone,
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
        prefix: None,
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
    // CrTokenType::CrNil => 29,
    // CrTokenType::CrOr => 30,
    // CrTokenType::CrPrint => 31,
    // CrTokenType::CrReturn => 32,
    // CrTokenType::CrSuper => 33,
    // CrTokenType::CrThis => 34,
    // CrTokenType::CrTrue => 35,
    // CrTokenType::CrVar => 36,
    // CrTokenType::CrWhile => 37,
    // CrTokenType::CrEof => 38,
    // CrTokenType::CrPrintln => 39,
    // CrTokenType::CrNonpert => 40,
    UnsafeCell::new(CraftParseRule {
        prefix: None,
        infix: None,
        precdc: Precedence::PrecNone,
    }),
    UnsafeCell::new(CraftParseRule {
        prefix: None,
        infix: None,
        precdc: Precedence::PrecNone,
    }),
    UnsafeCell::new(CraftParseRule {
        prefix: None,
        infix: None,
        precdc: Precedence::PrecNone,
    }),
    UnsafeCell::new(CraftParseRule {
        prefix: None,
        infix: None,
        precdc: Precedence::PrecNone,
    }),
    UnsafeCell::new(CraftParseRule {
        prefix: None,
        infix: None,
        precdc: Precedence::PrecNone,
    }),
    UnsafeCell::new(CraftParseRule {
        prefix: None,
        infix: None,
        precdc: Precedence::PrecNone,
    }),
    UnsafeCell::new(CraftParseRule {
        prefix: None,
        infix: None,
        precdc: Precedence::PrecNone,
    }),
    UnsafeCell::new(CraftParseRule {
        prefix: None,
        infix: None,
        precdc: Precedence::PrecNone,
    }),
    UnsafeCell::new(CraftParseRule {
        prefix: None,
        infix: None,
        precdc: Precedence::PrecNone,
    }),
    UnsafeCell::new(CraftParseRule {
        prefix: None,
        infix: None,
        precdc: Precedence::PrecNone,
    }),
    UnsafeCell::new(CraftParseRule {
        prefix: None,
        infix: None,
        precdc: Precedence::PrecNone,
    }),
    UnsafeCell::new(CraftParseRule {
        prefix: None,
        infix: None,
        precdc: Precedence::PrecNone,
    }),
    UnsafeCell::new(CraftParseRule {
        prefix: None,
        infix: None,
        precdc: Precedence::PrecNone,
    }),
    UnsafeCell::new(CraftParseRule {
        prefix: None,
        infix: None,
        precdc: Precedence::PrecNone,
    }),
] );

impl Debug for CraftParser<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CraftParser")
            .field("current", &self.current)
            .field("previous", &self.previous)
            .field("state", &self.state)
            .finish()
    }
}

#[allow(dead_code)]
impl<'a> CraftParser<'a> {

    pub fn new(tokseq: Seq<(CrTokenType<'a>, usize, usize)>, chnk: RefCell<CraftChunk>) -> Self {
        Self {
            tokseq,
            chnk,
            current:  RefCell::new(TokenData::default()),
            previous: RefCell::new(TokenData::default()),
            state:    RefCell::new(ParseState::default()),
        }
    }

    fn number(&mut self) -> Result<(), String> {
        println!("[debug] in number!");
        let tok = self.previous.borrow();
        match &tok.token {
            CrTokenType::CrNumber(ref value) => {
                println!("it was a number {value}");
                let mut curch = self.chnk.borrow_mut();
                // no need to check for max int sinze we use usize for the indexes
                curch.add_const(*value, tok.line);
                Ok(())
            }
            t => Err(format!("Not a number in {t:?}")),
        }
    }

    // This function compiles the right operand, much like how unary() compiles its
    // own trailing operand. Finally, it emits the bytecode instruction that
    // performs the binary operation.
    fn binary(&mut self) -> Result<(), String> {
        println!("[debug] in binary!");
        //let _rule = get_parse_rule(self.previous.borrow().token.order());
        unsafe {
            let _rule = PARSERULES.get(self.previous.borrow().token.order()).unwrap().get() ;
            self.parse_precedence(
                &((*_rule).precdc.next())
                //&PARSERULES.read().unwrap().get(self.previous.borrow().token.order()).unwrap().precdc.next()
            )?;
        }
        println!("[debug] we acquired in binary!");
        println!("back from precedence");
        //self.advance();
        let tok = &self.previous.borrow();
        let mut ch = self.chnk.borrow_mut();
        println!("pushing op {:?}", tok.token);
        match tok.token {
            CrTokenType::CrPlus => {
                ch.emit_byte(OpType::Simple(common::OpCode::OpAdd), tok.line);
                Ok(())
            }
            CrTokenType::CrMinus => {
                ch.emit_byte(OpType::Simple(common::OpCode::OpSub), tok.line);
                Ok(())
            }
            CrTokenType::CrStar => {
                ch.emit_byte(OpType::Simple(common::OpCode::OpMult), tok.line);
                Ok(())
            }
            CrTokenType::CrSlash => {
                ch.emit_byte(OpType::Simple(common::OpCode::OpDiv), tok.line);
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn unary(&mut self) -> Result<(), String> {
        println!("[debug] in unary!");
        self.parse_precedence(&Precedence::PrecUnary)?;
        let prev = self.previous.borrow();
        let mut ch = self.chnk.borrow_mut();
        match prev.token {
            // we emit the bytecode to perform the negation.
            // It might seem a little weird to write the negate instruction after
            // its operand’s bytecode since the - appears on the left,
            // but think about it in terms of order of execution:
            // We evaluate the operand first which leaves its value on the stack.
            // Then we pop that value, negate it, and push the result.
            CrTokenType::CrMinus => {
                ch.emit_byte(OpType::Simple(common::OpCode::OpNegate), prev.line);
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn parse_precedence(&mut self, _prec: &Precedence) -> Result<(), String> {
        println!("[debug] in precedence for {_prec:?}!");
        self.advance();
        //let prefrule = get_parse_rule(self.previous.borrow().token.order()); 
        let binding = unsafe { PARSERULES.get_mut(self.previous.borrow().token.order()).unwrap() };
        let prefrule = binding.get_mut(); 
        println!("applying rule");
        match prefrule.prefix {
            Some(ref mut _preffn) => {
                println!("found some prefix rule for that");
                match _preffn.as_mut()(self) {
                    Ok(n)  => n,
                    Err(e) => println!("Error from parse rule somewhere: {e}!!"),
                };
                // Now we look for an infix parser for the next token.
                // If we find one, it means the prefix expression we already compiled might be an operand for it.
                // But only if the call to parsePrecedence() has a precedence that is low enough to permit that infix operator.
                println!("looping over precedences");
                unsafe  {
                    while (_prec.order())
                    <= ((*PARSERULES.get(self.current.borrow().token.order()).unwrap().get()).precdc).order()
                    //<= (get_parse_rule(self.current.borrow().token.order()).precdc).order()
                    {
                        // we consume the operator and hand off control to the infix parser we found.
                        // It consumes whatever other tokens it needs (usually the right operand) and returns back to parsePrecedence().
                        // Then we loop back around and see if the next token is also a valid infix operator that can take the entire
                        // preceding expression as its operand.
                        self.advance();
                        //let nprefrule = &get_parse_rule(self.previous.borrow().token.order()).infix;
                        let binding: &mut UnsafeCell<CraftParseRule> = PARSERULES.get_mut(self.previous.borrow().token.order()).unwrap();
                        let nprefrule = binding.get_mut();
                        println!("we found the precedence rule: {:?}", nprefrule.precdc);
                        if let Some(ref mut _npreffn) = nprefrule.infix {
                            println!("another precedence rule found");
                            match _npreffn.as_mut()(self) {
                                Ok(n) => n,
                                Err(e) => println!("Error from parse rule somewhere: {e}!!"),
                            };
                            break;
                        } else {
                            println!("the prefix was empty??");
                        }
                    }
                }
                Ok(())
            }
            None => {
                //self.error_at(&self.previous.borrow(), "Expected expression");
                //Err("Expected expression".into())
                Ok(())
            }
        }
    }

    fn grouping(&mut self) -> Result<(), String> {
        println!("[debug] in grouping!");
        self.expression()?;
        self.consume(CrTokenType::CrRightParen, "Close grouping")
    }

    fn expression(&mut self) -> Result<(), String> {
        println!("[debug] in expression!");
        self.parse_precedence(&Precedence::PrecAssignment)?;
        println!("back to expression");
        Ok(())
    }

    pub fn advance(&mut self) -> bool {
        println!("[debug] advancing!!");
        self.previous.swap(&self.current);
        // unlike the book impl, we are re-using the scanner in ocaml so we don't handle special
        // tokens here!
        if let Some(tok) = self.tokseq.next() {
            match tok {
                Ok((token, line, col)) => {
                    self.current.replace(TokenData { line, col, token });
                    return true;
                }
                Err(e) => {
                    println!("Token Sequence error: {e:?}!");
                    self.error_at(&self.previous.borrow(), "TokenSequence Error");
                    return false;
                }
            }
        }; 
        false
    }

    pub fn consume(&mut self, expected: CrTokenType, message: &'static str) -> Result<(), String> {
        println!("[debug] consuming!!");
        if self.current.get_mut().token.eq(&expected) {
            self.advance();
            return Ok(());
        }
        self.error_at_curr(message);
        Err(message.into())
    }

    pub fn end(&mut self) {
        self.chnk.get_mut().end_compiler(self.current.borrow().line + 1);
    }

    pub fn error_at(&self, tok: &TokenData, message: &'static str) {
        let mut state = self.state.borrow_mut();
        let line = tok.line;
        let coln = tok.col;
        let tokn = &tok.token;
        if state.panic_md {
            println!("already in panic mode!!");
            return;
        }

        #[cfg(feature = "vmtrace")]
        {
            let ch = self.chnk.borrow();
            super::debug::disas("== dump ==", &ch, ch.into_iter());
        }

        state.panic_md = true;
        println!("Error at line ({line}, {coln}), tok: {tokn:?}, {message} ");
        if let CrTokenType::CrEof = tokn {
            println!("End of File!");
            return;
        };
        println!();
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
    while _parser.advance() {
        _parser.expression().unwrap();
    }
    _parser.consume(CrTokenType::CrEof, "expected End of File").unwrap();
    _parser.end();
    !_parser.state.borrow().had_err
}
