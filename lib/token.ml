type tokentype = 
    (* Single-character tokens. *)
    | LEFT_PAREN
    | RIGHT_PAREN 
    | LEFT_BRACE
    | RIGHT_BRACE
    | COMMA 
    | DOT 
    | MINUS 
    | PLUS 
    | SEMICOLON 
    | SLASH 
    | STAR
    (* One or two character tokens. *)
    | BANG
    | BANG_EQUAL
    | EQUAL 
    | EQUAL_EQUAL
    | GREATER
    | GREATER_EQUAL
    | LESS
    | LESS_EQUAL
    (* Literals. *)
    | IDENTIFIER 
    | STRING of string 
    | NUMBER of float
    (* Keywords.*)
    | AND 
    | CLASS 
    | ELSE 
    | FALSE 
    | FUN 
    | FOR 
    | IF 
    | NIL 
    | OR
    | PRINT 
    | RETURN 
    | SUPER 
    | THIS 
    | TRUE 
    | VAR 
    | WHILE
    | EOF
    (* Delim *)
    | NONPERT
    | MULTILINECOMMENT
[@@deriving show];;

type token = {
      ttype  :  tokentype
    ; col    :  int
}[@@deriving show];;

let mktoken ttyp col = 
    { ttype=ttyp; col }
;;


