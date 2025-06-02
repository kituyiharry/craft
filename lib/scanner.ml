open Effect;;
open Effect.Deep;;
open Token;;

type lines = (string * int) Seq.t;;
type chars = (char   * int) Seq.t;;

type _ Effect.t += 
    (* Effects on a single line *)
    | SkipLine: chars -> chars Effect.t        (* Skip the whole line *)
    | SkipNums: chars * int  -> chars Effect.t (* Skip n chars *)
    | Collect : chars * (char -> bool) -> (chars * bool * Buffer.t) Effect.t (* Collect until we find char *)
    (* Effects on multiple lines *)
    (*| MultilineComment*)
    (*| Advance of  ((Buffer.t * tokentype) * ((char * int) -> bool))*)
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

let parse_token tok seqst = 
    match tok with

    (* simple *)
    |  '(' -> (Ok (LEFT_PAREN ), seqst)
    |  ')' -> (Ok (RIGHT_PAREN), seqst)
    |  '{' -> (Ok (LEFT_BRACE ), seqst)
    |  '}' -> (Ok (RIGHT_BRACE), seqst)
    |  ',' -> (Ok (COMMA      ), seqst)
    |  '.' -> (Ok (DOT        ), seqst)
    |  '-' -> (Ok (MINUS      ), seqst)
    |  '+' -> (Ok (PLUS       ), seqst)
    |  ';' -> (Ok (SEMICOLON  ), seqst)
    |  '*' -> (Ok (STAR       ), seqst)

    (* grouped *)
    |  '!' -> (match next seqst '=' with
            | (true,  seqst') -> (Ok (BANG_EQUAL), seqst')
            | (false, seqst') -> (Ok (BANG), seqst')
        )
    | '=' -> (match next seqst '=' with
            | (true,  seqst') -> (Ok (EQUAL_EQUAL), seqst')
            | (false, seqst') -> (Ok (EQUAL), seqst')
        )
    | '<' -> (match next seqst '=' with
            | (true,  seqst') -> (Ok (LESS_EQUAL), seqst')
            | (false, seqst') -> (Ok (LESS), seqst')
        )
    | '>' -> (match next seqst '=' with
            | (true,  seqst') -> (Ok (GREATER_EQUAL), seqst')
            | (false, seqst') -> (Ok (GREATER), seqst')
        )

    (* ignored *)
    | '/' -> (match next seqst '/' with
            | (true, seqst') ->
                (* or just drop the whole sequence as this is a comment *)
                (Ok NONPERT, perform (SkipLine seqst'))
            | (false, seqst') -> 
                (Ok SLASH, seqst')
        )
    | ' '  -> (Ok NONPERT, perform (SkipNums (seqst, 0)))
    | '\t' -> (Ok NONPERT, perform (SkipNums (seqst, 0)))
    | '\r' -> (Ok NONPERT, perform (SkipNums (seqst, 0)))
    | '\n' -> (Ok NONPERT, perform (SkipNums (seqst, 0)))

    (* single line strings *)
    | '"' ->  
        let more, term, buf = perform (Collect (seqst, (fun ch -> ch != '"'))) in
        if term then
            (* make sure to drop the terminating quote *)
            (Ok (STRING (Buffer.contents buf)), Seq.drop 1 more)
        else
            (Error "Unterminated string!", seqst)

    (* unmatched - halt parser *)
    |   c  -> 
        if isDigit(c) then
            let mbuf = Buffer.create 10 in
            let _ = Buffer.add_char mbuf c in
            let more, _term, buf = perform (Collect (seqst, (fun ch -> isDigit ch))) in 
            let _ = Buffer.add_buffer mbuf buf in
            let _ = Seq.iter (fun (c, _) -> Format.print_char c) more in
            (match next more '.' with
                | (true,  seqst') -> 
                    let more', _term, buf' = perform (Collect (seqst', (fun ch -> isDigit ch)))
                    in 
                    
                    if Buffer.length buf' = 0 then
                        (Error ("Ambigous: Number cannot terminate with period e.g <num>.<num> or <num>.(function)"), more')
                    else
                        let _ = Buffer.add_char mbuf '.' in
                        let _ = Buffer.add_buffer mbuf buf' in
                        let cont = Buffer.contents mbuf in
                        (try
                            let flt = Float.of_string cont in
                            (Ok (NUMBER flt), more') 
                        with
                            | Failure s -> 
                            (Error (Format.sprintf "Invalid number: %s (%s)" cont s), more')
                        )


                | (false, seqst') -> 
                    let cont = (Buffer.contents mbuf) in
                    try
                        let flt = Float.of_string cont in
                        (Ok (NUMBER flt), seqst') 
                    with
                        | Failure s -> 
                        (Error (Format.sprintf "Invalid number: %s (%s)" cont s), seqst')
            )
        else
            (Error (Format.sprintf "unhandled token: %c" c), seqst) 
;;

let scan_tokens cursor charseq = 
    (* zip with col numbers *)
    let cseq = (Seq.zip charseq (Seq.ints 0)) in
    let rec chars cseq state =
        match Seq.uncons cseq with
        | None -> 
            cursor, Ok (List.rev state)
        | Some ((tok, col), more) ->
            try 
                let (lexeme', more') = parse_token tok more in
                match lexeme' with
                | Ok NONPERT ->
                    chars (more') (state)
                | Ok lexeme ->
                    chars (more') ((Token.mktoken lexeme col) :: state)
                | Error _ as e -> 
                    cursor, e
            with
                (* single line comment has the effect of ignoring until a newline is reached *)
                | effect (SkipLine _more'), k ->
                continue k (Seq.empty)
                | effect (SkipNums (more', num)), k -> 
                continue k (Seq.drop num more')
                | effect (Collect (more', predc)), k -> 
                let buf = Buffer.create 40 in
                let term = ref false in
                let _ = Seq.iter (Buffer.add_char buf) 
                    @@ Seq.map (fst) 
                    @@ Seq.take_while (fun (ch,_) -> 
                        let _ = term := predc ch in 
                        !term
                    ) more' in
                let size = Buffer.length buf in
                let rems = Seq.drop size more' in
                continue k (rems, not !term, buf)
    in chars cseq []
;;

(* consume chars to tokens -> processes a line *)
let scan_lines lineseq = 
    (* zip with line numbers *)
    let lseq = (Seq.zip lineseq (Seq.ints 0)) in 
    let rec lines lseq state = 
        match Seq.uncons lseq with
        | Some ((line, num), more) -> 
            let cursor', lexemes = scan_tokens more (line |> String.to_seq) in
            (match lexemes with
                | Ok lexemes' -> 
                    let state' = Ok (num, lexemes') :: state in
                    lines cursor' state'
                | Error e -> 
                    let state' = Error (num, e) :: state in
                    lines cursor' state'
            )
        | None -> 
            List.rev state 
    in lines lseq []
;;

