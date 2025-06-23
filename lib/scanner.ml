open Effect;;
open Effect.Deep;;
open Token;;

type lines = (string * int) Seq.t;;
type chars = (char   * int) Seq.t;;

type lexout = {
        errs: (int * ((int * string) list)) list (* line * ((col * errmsg) list) *)
    ;   toks: (int * token list) list            (* line * tokens *)
} [@@deriving show];;

(* Store keywords *)
module Kt = Hashtbl.Make(String) ;;

type _ Effect.t += 
    (* Effects on a single line *)
    | SkipLine: chars -> chars Effect.t         (* Skip the whole line *)
    | SkipChars: chars * int  -> chars Effect.t (* Skip n chars *)
    | Collect : chars * (char -> bool) -> (chars * bool * Buffer.t) Effect.t (* Collect until we find char *)
    (* Effects on multiple lines *)
    | SkipAcross: lines * ((string * int) -> (bool * chars)) -> (bool * lines * chars) Effect.t
;;

let error line col message = 
    Format.sprintf "[line: %d, col: %d] error: %s" line col message 
;;

(* A variation of Seq.drop_while but it drops until 2 conditions in a row are
   satisfied and returns a match *)
let rec drop_til_follows p1 p2 xs =
    match xs () with
    | Seq.Nil ->
        (false, Seq.Nil)
    | Seq.Cons (x, xs) ->
        if p1 x then 
            drop_til_follows p1 p2 xs 
        else 
            match xs () with
            | Seq.Nil -> 
                (false, Seq.Nil) 
            | Seq.Cons (x', xs') as node' -> 
                if p2 x' then
                    drop_til_follows p1 p2 xs' 
                else
                    (true, node')
;;

(* match next token *)
let next tseq term = 
    match Seq.uncons tseq with
    | Some ((chr, _pos), more) -> (chr = term, more)
    | None -> (false, tseq)
;;

let isDigit c =
    c >= '0' && c <= '9'
;;

let isAlpha c =
    (c >= 'a' && c <= 'z') ||
    (c >= 'A' && c <= 'Z') ||
    (c == '_');
;;

let isAlphaNumeric c =
  isAlpha(c) || isDigit(c);
;;

let ht = 
    let h = Kt.create 15 in
    let _ = [
        ("and"   , AND   );
        ("class" , CLASS );
        ("else"  , ELSE  );
        ("false" , FALSE );
        ("for"   , FOR   );
        ("fun"   , FUN   );
        ("if"    , IF    );
        ("nil"   , NIL   );
        ("or"    , OR    );
        ("print" , PRINT );
        ("return", RETURN);
        ("super" , SUPER );
        ("this"  , THIS  );
        ("true"  , TRUE  );
        ("var"   , VAR   );
        ("while" , WHILE );
        (*_ -> IDENTIFIER k *)
    ] |> List.iter (fun (k, v) -> Kt.add h k v) in 
    h
;;

let keyword k =
    match Kt.find_opt ht k with
    | Some t -> t 
    | None -> IDENTIFIER k
;;

let parse_token tok lseqst cseqst = 
    match tok with

    (* simple *)
    |  '(' -> (Ok (LEFT_PAREN ), lseqst, cseqst)
    |  ')' -> (Ok (RIGHT_PAREN), lseqst, cseqst)
    |  '{' -> (Ok (LEFT_BRACE ), lseqst, cseqst)
    |  '}' -> (Ok (RIGHT_BRACE), lseqst, cseqst)
    |  ',' -> (Ok (COMMA      ), lseqst, cseqst)
    |  '.' -> (Ok (DOT        ), lseqst, cseqst)
    |  '-' -> (Ok (MINUS      ), lseqst, cseqst)
    |  '+' -> (Ok (PLUS       ), lseqst, cseqst)
    |  ';' -> (Ok (SEMICOLON  ), lseqst, cseqst)
    |  '*' -> (Ok (STAR       ), lseqst, cseqst)

    (* grouped *)
    |  '!' -> (match next cseqst '=' with
            | (true,  seqst') -> (Ok (BANG_EQUAL), lseqst, seqst')
            | (false, seqst') -> (Ok (BANG), lseqst, seqst')
        )
    | '=' -> (match next cseqst '=' with
            | (true,  seqst') -> (Ok (EQUAL_EQUAL), lseqst, seqst')
            | (false, seqst') -> (Ok (EQUAL),  lseqst, seqst')
        )
    | '<' -> (match next cseqst '=' with
            | (true,  seqst') -> (Ok (LESS_EQUAL), lseqst, seqst')
            | (false, seqst') -> (Ok (LESS),  lseqst, seqst')
        )
    | '>' -> (match next cseqst '=' with
            | (true,  seqst') -> (Ok (GREATER_EQUAL), lseqst, seqst')
            | (false, seqst') -> (Ok (GREATER), lseqst, seqst')
        )

    (* ignored *)
    | '/' -> (match next cseqst '/' with
            | (true, seqst') ->
                (* or just drop the whole sequence as this is a comment *)
                (Ok NONPERT, lseqst, perform (SkipLine seqst'))
            | (false, seqst') -> 
                (match next cseqst '*' with
                    | (true, seqst'') ->
                        let notstar  = Fun.compose ((!=) '*') fst in
                        let notslash = Fun.compose ((!=) '/') fst in
                        let found, node = drop_til_follows notstar notslash seqst'' in
                        if found then
                            (Ok NONPERT, lseqst, Seq.drop 1 (fun () -> node))
                        else
                            let (x, y, z) = perform (SkipAcross (
                                lseqst, 
                                (fun (l,_)-> 
                                    let x, y = drop_til_follows notstar notslash (Seq.zip (String.to_seq l) (Seq.ints 0))
                                    in x, (fun () -> y)
                                )
                            )) in
                            if x then
                                (Ok NONPERT, y, Seq.drop 1 z)
                            else
                                (Error "Unterminated multiline comment!!", lseqst, seqst')
                    | _ ->
                        (Ok SLASH, lseqst, seqst')
                )
        )
    | ' '  -> (Ok NONPERT, lseqst, perform (SkipChars (cseqst, 0)))
    | '\t' -> (Ok NONPERT, lseqst, perform (SkipChars (cseqst, 0)))
    | '\r' -> (Ok NONPERT, lseqst, perform (SkipChars (cseqst, 0)))
    | '\n' -> (Ok NONPERT, lseqst, perform (SkipChars (cseqst, 0)))

    (* single line strings *)
    | '"' ->  
        let more, term, buf = perform (Collect (cseqst, ((!=) '"'))) in
        if term then
            (* make sure to drop the terminating quote *)
            (Ok (STRING (Buffer.contents buf)), lseqst, Seq.drop 1 more)
        else
            (Error "Unterminated string!", lseqst,cseqst)

    (* unmatched - halt parser *)
    |   c  -> 
        if isDigit(c) then
            let mbuf = Buffer.create 10 in
            (* add current char and collect the rest *)
            let _ = Buffer.add_char mbuf c in
            let more, _term, buf = perform (Collect (cseqst, (isDigit))) in 
            let _ = Buffer.add_buffer mbuf buf in
            (match next more '.' with
                | (true,  seqst') -> 
                    let more', _term, buf' = perform (Collect (seqst', (isDigit)))
                    in 
                    if Buffer.length buf' = 0 then
                        (Error ("Ambigous use of period: Number cannot terminate with period
                        e.g <num>.<num> or <num>.(function)"), lseqst, more')
                    else
                        let _ = Buffer.add_char mbuf '.' in
                        let _ = Buffer.add_buffer mbuf buf' in
                        let cont = Buffer.contents mbuf in
                        (try
                            let flt = Float.of_string cont in
                            (Ok (NUMBER flt), lseqst, more') 
                        with
                            | Failure s -> 
                            (Error (Format.sprintf "Invalid number: %s (%s)"
                                cont s), lseqst, more')
                        )

                | (false, _seqst') -> 
                    let cont = (Buffer.contents mbuf) in
                    try
                        let flt = Float.of_string cont in
                        (Ok (NUMBER flt), lseqst, more) 
                    with
                        | Failure s -> 
                        (Error (Format.sprintf "Invalid number: %s (%s)" cont s), lseqst, more)
            )
        else if isAlpha c then 
            let mbuf = Buffer.create 10 in
            let _ = Buffer.add_char mbuf c in
            let more, _term, buf = perform (Collect (cseqst, (isAlphaNumeric))) in 
            let _ = Buffer.add_buffer mbuf buf in
            (Ok (keyword (Buffer.contents mbuf)), lseqst, more) 
        else
            (Error (Format.sprintf "unhandled token: %c" c), lseqst, cseqst) 
;;

let scan_tokens lines charseq = 
    (* zip with col numbers *)
    let cseq = (Seq.zip charseq (Seq.ints 0)) in
    let rec chars lines cseq state errs =
        match Seq.uncons cseq with
        | None -> 
            (List.rev state, List.rev errs)
        | Some ((tok, col), more) ->
            try 
                let (lexeme', lines', more') = parse_token tok lines more in
                match lexeme' with
                | Ok NONPERT ->
                    chars lines' (more') (state) errs
                | Ok lexeme ->
                    chars lines' (more') ((Token.mktoken lexeme col) :: state) errs
                | Error e -> 
                    chars lines' (more') (state) (((col, e)) :: errs)
            with
                (* single line comment has the effect of ignoring until a newline is reached *)
                | effect (SkipLine _more'), k ->
                    continue k (Seq.empty)
                | effect (SkipChars (more', num)), k -> 
                    continue k (Seq.drop num more')
                | effect (Collect (more', predc)), k -> 
                    (* todo: delay buffering util we find the actual value *)
                    let buf = Buffer.create 40 in
                    let term = ref false in
                    let _ = 
                        Seq.iter (Buffer.add_char buf) 
                        @@ Seq.map (fst) 
                        @@ Seq.take_while (fun (ch,_) -> 
                            let _ = term := predc ch in 
                            !term
                        ) more' in
                    let size = Buffer.length buf in
                    let rems = Seq.drop size more' in
                    continue k (rems, not !term, buf)
    in chars lines cseq [] []
;;

(* consume chars to tokens -> processes a line *)
let scan_lines lineseq = 
    (* zip with line numbers - start count from 1 *)
    let lseq = (Seq.zip lineseq (Seq.ints 1)) in 
    let rec lines lseq state errs = 
        match Seq.uncons lseq with
        | Some ((line, num), more) -> 
            (try 
                let (lexemes', tokerrs) = scan_tokens more (line |> String.to_seq) in
                let state' = (if List.is_empty lexemes' then state else
                    (num, lexemes') :: state 
                ) in
                let errs'  = (if List.is_empty tokerrs then errs else 
                    (num, tokerrs) :: errs
                ) in 
                lines more state' errs'
            with 
                | effect (SkipAcross (more', predc)), k -> 
                    let rec drop_to x = 
                        (match Seq.uncons x with 
                            | Some (l, m) -> 
                                let y, z = predc l in
                                if y then
                                    y, z, m
                                else
                                    drop_to m 
                            | None -> 
                                false, Seq.empty, Seq.empty
                        )
                    in 
                    let (y, z, m) = drop_to more' in
                    continue k (y, m, z))
        | None -> 
            { toks = (List.rev state); errs = (List.rev errs); }
    in lines lseq [] []
;;

