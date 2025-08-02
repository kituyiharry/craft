// Extracted from craft/lib/token.ml
#[derive(Debug, Clone, PartialEq, ocaml::ToValue, ocaml::FromValue)]
pub enum CrTokenType<'a> {
    //(* Single-character tokens. *)
    CrLeftParen,
    CrRightParen,
    CrLeftBrace,
    CrRightBrace,
    CrComma,
    CrDot,
    CrMinus,
    CrPlus,
    CrSemicolon,
    CrSlash,
    CrStar,
    //(* One or two character tokens. *)
    CrBang,
    CrBangEqual,
    CrEqual,
    CrEqualEqual,
    CrGreater,
    CrGreaterEqual,
    CrLess,
    CrLessEqual,
    //(* Literals. *)
    CrIdentifier(&'a str),
    CrString(&'a str),
    CrNumber(f64),
    //Keywords.*),
    CrAnd,
    CrClass,
    CrElse,
    CrFalse,
    CrFun,
    CrFor,
    CrIf,
    CrNil,
    CrOr,
    CrPrint,
    CrPrintln,
    CrReturn,
    CrSuper,
    CrThis,
    CrTrue,
    CrVar,
    CrWhile,
    CrEof,
    //(* Delim *)
    CrNonpert,
    //(* Experimental call memoization *)
    CrAtmemo,
}

//rust enums aren't like C and repr[u8] may interfere with ocaml ffi
impl CrTokenType<'_> {
    pub fn order(&self) -> usize {
        match *self {
            CrTokenType::CrLeftParen => 0,
            CrTokenType::CrRightParen => 1,
            CrTokenType::CrLeftBrace => 2,
            CrTokenType::CrRightBrace => 3,
            CrTokenType::CrComma => 4,
            CrTokenType::CrDot => 5,
            CrTokenType::CrMinus => 6,
            CrTokenType::CrPlus => 7,
            CrTokenType::CrSemicolon => 8,
            CrTokenType::CrSlash => 9,
            CrTokenType::CrStar => 10,
            CrTokenType::CrBang => 11,
            CrTokenType::CrBangEqual => 12,
            CrTokenType::CrEqual => 13,
            CrTokenType::CrEqualEqual => 14,
            CrTokenType::CrGreater => 15,
            CrTokenType::CrGreaterEqual => 16,
            CrTokenType::CrLess => 17,
            CrTokenType::CrLessEqual => 18,
            CrTokenType::CrIdentifier(_) => 19,
            CrTokenType::CrString(_) => 20,
            CrTokenType::CrNumber(_) => 21,
            CrTokenType::CrAnd => 22,
            CrTokenType::CrClass => 23,
            CrTokenType::CrElse => 24,
            CrTokenType::CrFalse => 25,
            CrTokenType::CrFor => 26,
            CrTokenType::CrFun => 27,
            CrTokenType::CrIf => 28,
            CrTokenType::CrNil => 29,
            CrTokenType::CrOr => 30,
            CrTokenType::CrPrint => 31,
            CrTokenType::CrReturn => 32,
            CrTokenType::CrSuper => 33,
            CrTokenType::CrThis => 34,
            CrTokenType::CrTrue => 35,
            CrTokenType::CrVar => 36,
            CrTokenType::CrWhile => 37,
            CrTokenType::CrEof => 38,
            CrTokenType::CrPrintln => 39,
            CrTokenType::CrNonpert => 40,
            CrTokenType::CrAtmemo => 41,
        }
    }
}

impl PartialOrd for CrTokenType<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.order().cmp(&other.order()))
    }
}
