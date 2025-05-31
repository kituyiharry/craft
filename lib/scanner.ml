open Token;;

type scanner = {
      source: char Seq.t [@opaque] 
    ; tokens: token list 
}[@@deriving show];;

(* TODO: use effects ?? *)
type errparse = 
    | Unrecognized of string 
    | Comment 
    | MultilineComment
    | NonPertinent
    | Advance of  ((Buffer.t * tokentype) * ((char * int) -> bool))
;;

(* match next token *)
let next tseq term = 
    match Seq.uncons tseq with
    | Some ((chr, _pos), more) -> (chr = term, more)
    | None -> (false, tseq)
;;

let parse_token tok seqst = 
    match tok with

    (* simple *)
    |  '(' -> Ok (LEFT_PAREN , seqst)
    |  ')' -> Ok (RIGHT_PAREN, seqst)
    |  '{' -> Ok (LEFT_BRACE , seqst)
    |  '}' -> Ok (RIGHT_BRACE, seqst)
    |  ',' -> Ok (COMMA      , seqst)
    |  '.' -> Ok (DOT        , seqst)
    |  '-' -> Ok (MINUS      , seqst)
    |  '+' -> Ok (PLUS       , seqst)
    |  ';' -> Ok (SEMICOLON  , seqst)
    |  '*' -> Ok (STAR       , seqst)

    (* grouped *)
    |  '!' -> (match next seqst '=' with
            | (true,  seqst') -> Ok (BANG_EQUAL, seqst')
            | (false, seqst') -> Ok (BANG, seqst')
        )
    | '=' -> (match next seqst '=' with
            | (true,  seqst') -> Ok (EQUAL_EQUAL, seqst')
            | (false, seqst') -> Ok (EQUAL,       seqst')
        )
    | '<' -> (match next seqst '=' with
            | (true,  seqst') -> Ok (LESS_EQUAL, seqst')
            | (false, seqst') -> Ok (LESS,       seqst')
        )
    | '>' -> (match next seqst '=' with
            | (true,  seqst') -> Ok (GREATER_EQUAL, seqst')
            | (false, seqst') -> Ok (GREATER,       seqst')
        )

    (* ignored *)
    | '/' -> (match next seqst '/' with
            | (true, _) ->
                (* or just drop the whole sequence as this is a comment *)
                Error ((Comment, Seq.empty))
            | (false, seqst') -> 
                Ok (SLASH, seqst')
        )
    | ' '  ->  Error (NonPertinent, seqst)
    | '\t' ->  Error (NonPertinent, seqst)
    | '\r' ->  Error (NonPertinent, seqst)
    | '\n' ->  Error (NonPertinent, seqst)

    (* long string *)
    | '"' -> 
        let buf = Buffer.create 80 in
        Error (Advance ((buf, STRING), 
            (Fun.compose  ((!=) '"') fst)
        ), seqst)

    (* unmatched - halt parser *)
    |   c  -> Error (Unrecognized (Format.sprintf "Unmatched token: %c" c), seqst)
;;

(* consume chars to tokens -> processes a line *)
let scan_tokens charseq = 
    let rec tokscan lineno tokseq' state = 
        match Seq.uncons (tokseq') with 
        | None -> 
            let _ = Format.printf "no state!" in
            Ok state 
        | Some ((chr, col), more) -> 
            let _ = Format.printf "found char: %c" chr in
            match parse_token chr more with
            | Ok (lexeme, more') -> 
                let _ = Format.printf "found token: %s" (show_tokentype lexeme) in 
                tokscan (lineno + 1) (more') ((Token.mktoken lexeme `Empty lineno col) :: state)
                (* TODO: Can be effects? *)
            | Error (parse_err, more') -> 
                let _ = Format.printf "error token!" in 
                (match parse_err with
                (* ignore rest of the line sequence *)
                | Comment -> 
                    let _ = Format.printf "Comment!\n" in 
                    tokscan (lineno + 1) more' state
                | MultilineComment ->
                    (* todo handle multiple lines *)
                    let _ = Format.printf "Multiline Comment!\n" in 
                    tokscan (lineno + 1) more' state
                | NonPertinent -> 
                    let _ = Format.printf "NonPertinent!\n" in 
                    tokscan (lineno + 1) more' state
                | Unrecognized ch -> 
                    Error (col, Format.sprintf "{ParseError: `%s`}" ch)
                | Advance ((buf, tok), clos) -> 
                    let _ = Format.printf "Advance tok!\n" in 
                    let _ = Buffer.add_string buf "" in
                    let _ = 
                        Buffer.add_seq buf @@ Seq.map (fst) @@
                        Seq.take_while (clos) more' in
                    let conts = Buffer.contents buf in
                    (*Ok ((Token.mktoken tok (`Obj (conts)) lineno col) :: state) *)
                    tokscan (lineno+1) (more') ((Token.mktoken tok (`Obj (conts)) lineno col) :: state)
            )
    in tokscan 0 charseq []
;;

