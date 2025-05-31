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
    | STRING 
    | NUMBER
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
[@@deriving show];;

type token = {
      ttype  :  tokentype
    (*; lexeme :  bytes*)
    ; literal:  [`Empty | `Obj of string]
    ; line   :  int
    ; col    :  int
}[@@deriving show];;

let mktoken ttyp lit line col = 
    { ttype=ttyp; literal=lit; line; col }
;;


