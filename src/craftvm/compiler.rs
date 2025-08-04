#![allow(static_mut_refs)]

use super::{chunk::CraftChunk, common, scanner::CrTokenType};
use crate::craftvm::{common::OpType, value::CraftValue};
use log::log;
use ocaml::Seq;
use once_cell::sync::Lazy;
use std::{
    cell::{RefCell, UnsafeCell},
    fmt::Debug,
};

pub type TokSeqItem<'a> = (CrTokenType<'a>, usize, usize);

#[derive(Debug, Default)]
pub struct ParseState {
    pub had_err: bool,
    pub panic_md: bool,
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
    pub state: RefCell<ParseState>,
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
// You can’t start an expression with, say, `else`, and `}` would make for a pretty confusing infix operator
//
#[allow(clippy::redundant_closure)]
static mut PARSERULES: Lazy<[UnsafeCell<CraftParseRule>; 42]> = Lazy::new(|| {
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
            infix: Some(Box::new(|s| CraftParser::binary(s))),
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
            infix: Some(Box::new(|s| CraftParser::binary(s))),
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
            prefix: Some(Box::new(|s| CraftParser::unary(s))),
            infix:  None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrBangEqual => 12,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix:  Some(Box::new(|s| CraftParser::binary(s))),
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
            infix:  Some(Box::new(|s| CraftParser::binary(s))),
            precdc: Precedence::PrecEquality,
        }),
        // CrTokenType::CrGreater => 15,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix:  Some(Box::new(|s| CraftParser::binary(s))),
            precdc: Precedence::PrecComparison,
        }),
        // CrTokenType::CrGreaterEqual => 16,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix:  Some(Box::new(|s| CraftParser::binary(s))),
            precdc: Precedence::PrecComparison,
        }),
        // CrTokenType::CrLess => 17,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix:  Some(Box::new(|s| CraftParser::binary(s))),
            precdc: Precedence::PrecComparison,
        }),
        // CrTokenType::CrLessEqual => 18,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix:  Some(Box::new(|s| CraftParser::binary(s))),
            precdc: Precedence::PrecComparison,
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
            prefix: Some(Box::new(|s| CraftParser::literal(s))),
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
            prefix: Some(Box::new(|s| CraftParser::literal(s))),
            infix:  None,
            precdc: Precedence::PrecNone,
        }),
        // CrTokenType::CrOr => 30,
        UnsafeCell::new(CraftParseRule {
            prefix: None,
            infix: None,
            precdc: Precedence::PrecNone,
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
            prefix: Some(Box::new(|s| CraftParser::literal(s))),
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

    fn number(&mut self) -> ParseRs {
        log::debug!("accepted a number!");
        let tok = self.previous.borrow();
        match &tok.token {
            // no need to check for max int sinze we use usize for the indexes
            CrTokenType::CrNumber(ref value) => {
                log::debug!("was number {value}");
                self.chnk.borrow_mut().add_const(CraftValue::CrNumber(*value), tok.line);
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
    fn binary(&mut self) -> ParseRs {
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
        let mut ch = self.chnk.borrow_mut();
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

    fn literal(&mut self) -> ParseRs {
        log::debug!("accepted literal expr!");
        let prev = self.previous.borrow();
        let mut ch = self.chnk.borrow_mut();
        match &prev.token {
            CrTokenType::CrFalse => ch.emit_byte(OpType::Simple(common::OpCode::OpFalse), prev.line),
            CrTokenType::CrTrue  => ch.emit_byte(OpType::Simple(common::OpCode::OpTrue),  prev.line),
            CrTokenType::CrNil   => ch.emit_byte(OpType::Simple(common::OpCode::OpNil),   prev.line),
            s => log::error!("unhandled literal {s:?}"),
        }
        Ok(())
    }

    fn unary(&mut self) -> ParseRs {
        log::debug!("accepted unary expr!");
        let prevt = self.previous.borrow().token.clone();
        let line  = self.previous.borrow().line;
        self.parse_precedence(&Precedence::PrecUnary)?;
        let mut ch = self.chnk.borrow_mut();
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
                match _preffn(self) {
                    Ok(n) => n,
                    Err(e) => println!("Error from parse rule somewhere: {e}!!"),
                };
                // Now we look for an infix parser for the next token.
                // If we find one, it means the prefix expression we already compiled might be an operand for it.
                // But only if the call to parsePrecedence() has a precedence that is low enough to permit that infix operator.
                let order = _prec.order();
                unsafe {
                    while order <= ((*PARSERULES.get(self.current.borrow().token.order()).unwrap().get())
                        .precdc).order()
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
                            match _npreffn(self) {
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
                log::error!("the prefix rule was empty");
                Ok(())
            }
        }
    }

    fn grouping(&mut self) -> ParseRs {
        log::debug!("accepted grouping!");
        self.expression()?;
        self.consume(CrTokenType::CrRightParen, "Close grouping")
    }

    fn expression(&mut self) -> ParseRs {
        log::debug!("accepted raw expression!");
        self.parse_precedence(&Precedence::PrecAssignment)?;
        Ok(())
    }

    pub fn advance(&mut self) -> bool {
        log::debug!("advancing!!");
        self.previous.swap(&self.current);
        // unlike the book impl, we are re-using the scanner in ocaml so we don't handle special
        // tokens here!
        if let Some(tok) = self.tokseq.next() {
            match tok {
                Ok((token, line, col)) => {
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

    pub fn consume(&mut self, expected: CrTokenType, message: &'static str) -> ParseRs {
        log::debug!("consuming a token, expecting {expected:?}!");
        if self.current.get_mut().token.eq(&expected) {
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
            .end_compiler(self.current.borrow().line + 1);
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
            let ch = self.chnk.borrow();
            super::debug::disas("== dump ==", &ch, ch.into_iter());
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
    while _parser.advance() {
        _parser.expression().unwrap();
        log::info!("compiler advancing expression");
    }
    //_parser.consume(CrTokenType::CrEof, "expected End of File").unwrap();
    _parser.end();
    !_parser.state.borrow().had_err
}
