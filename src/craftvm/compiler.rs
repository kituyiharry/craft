use std::cell::RefCell;
use ocaml::Seq;
use super::{chunk::CraftChunk, scanner::CrTokenType};

pub type TokSeqItem = (CrTokenType, usize, usize);

#[derive(Debug)]
#[derive(Default)]
pub struct ParseState {
    had_err : bool, 
    panic_md: bool, 
}

#[derive(Debug)]
pub struct TokenData { 
    pub line : usize, 
    pub col  : usize, 
    pub token: CrTokenType, 
}

impl Default for TokenData {
    fn default() -> Self {
        Self { line: 0, col: 0, token: CrTokenType::CrEof }
    }
}

// NB: if previous is EoF, then current 
pub struct CraftParser {
    tokseq  : Seq<(CrTokenType, usize, usize)>,
    current : RefCell<TokenData>,
    previous: RefCell<TokenData>,
    state   : RefCell<ParseState>,
}

impl CraftParser {

    pub fn new(tseq: Seq<(CrTokenType, usize, usize)>) -> Self {
        Self { 
            tokseq:   tseq,
            current:  RefCell::new(TokenData::default()), 
            previous: RefCell::new(TokenData::default()), 
            state:    RefCell::new(ParseState::default()), 
        }
    }

    pub fn advance(&mut self) {
        self.previous.swap(&self.current);
        // unlike the book impl, we are re-using the scanner in ocaml so we don't handle special
        // tokens here!
        if let Some(tok) = self.tokseq.next() {
            match tok {
                Ok((token, line, col)) => {
                    self.current.replace(TokenData { line, col, token });
                }
                Err(e) => {
                    println!("Token Sequence error: {e:?}!");
                    self.error_at(&self.previous.borrow(), "TokenSequence Error");
                }
            }
        }
    }

    pub fn consume(&mut self, expected: CrTokenType, message: &'static str) {
        if self.current.get_mut().token.eq(&expected) {
            self.advance();
            return;
        }
        self.error_at_curr(message);
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
        state.panic_md = true;
        println!("Error at line ({line}, {coln}), tok: {tokn:?}, {message} ");
        if let CrTokenType::CrEof = tokn {
            println!("End of File!");
            return;
        };
        println!();
        state.had_err  = true;
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

pub fn compile(_chunk: &'_ CraftChunk, _parser: &'_ CraftParser) -> bool {
    _parser.state.borrow().had_err
}
