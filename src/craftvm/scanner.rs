// Extracted from craft/lib/token.ml 
#[derive(Debug)]
#[derive(ocaml::ToValue, ocaml::FromValue)]
pub enum CrTokenType { 
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
    CrIdentifier(String),
    CrString(String),
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

