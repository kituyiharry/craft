open Ast

let (let*) = Result.bind;; 

let rec eval = function 
    | Literal l   -> Ok l
    | Grouping g  -> eval g
    | Unhandled (e, _, _, _) -> Error (Format.sprintf "invalid expression: %s!!" (show_expr (Option.value ~default:(Literal Nil) e)))
    | Unary (op, u) -> 
        let* u' = eval u in
        (match op with
            | Negate -> (match u' with 
                    | Number f -> (Ok (Number (Float.neg f))) 
                    | n -> Error (Format.sprintf "attempt to negate %s" (show_lit n))
                )
            | Invert -> (match u' with 
                    | Bool b -> Ok (Bool (Bool.not b)) 
                    | n -> Error (Format.sprintf "attempt to Invert %s" (show_lit n))
                )
        )
    | Binary   (l, op, r) -> 
        let* l' = eval l in
        let* r' = eval r in
        (match op with
            | Compr o -> (match o with
                | Greater    -> (match (l', r') with
                    | (Number l'', Number r'') -> Ok (Bool ((Float.compare l'' r'') = 1)) 
                    | (x, y) -> 
                        Error ((Format.sprintf "invalid bool operands: %s = %s" (show_lit x) (show_lit y)))
                    )
                | GreaterEq -> (match (l', r') with
                    | (Number l'', Number r'') ->
                        let x = Float.compare l'' r'' in
                        Ok (Bool (x = 1 || x = 0)) 
                    | (x, y) -> 
                        Error ((Format.sprintf "invalid bool operands: %s != %s" (show_lit x) (show_lit y)))
                    )
                | Lesser -> (match (l', r') with
                    | (Number l'', Number r'') -> Ok (Bool ((Float.compare l'' r'') = -1)) 
                    | (x, y) -> 
                        Error ((Format.sprintf "invalid bool operands: %s != %s" (show_lit x) (show_lit y)))
                    )
                | LesserEq -> (match (l', r') with
                    | (Number l', Number r') -> 
                        let x = Float.compare l' r' in
                        Ok (Bool (x = -1 || x = 0)) 
                    | (x, y) -> 
                        Error ((Format.sprintf "invalid bool operands: %s != %s" (show_lit x) (show_lit y)))
                    )
                ) 
            | Operator o -> (match o with
                | Eq    -> (match ( l',  r') with
                    | (Bool   l'', Bool   r'') -> Ok (Bool ((=) l'' r'')) 
                    | (Number l'', Number r'') -> Ok (Bool (Float.equal l'' r''))
                    | (x, y) -> 
                        Error ((Format.sprintf "invalid bool operands: %s = %s" (show_lit x) (show_lit y)))
                    )
                | NotEq -> (match ( l',  r') with
                    | (Bool   l'', Bool   r'') -> Ok (Bool ((!=) l'' r'')) 
                    | (Number l'', Number r'') -> Ok (Bool (not @@ Float.equal l'' r'')) 
                    | (x, y) -> 
                        Error ((Format.sprintf "invalid bool operands: %s != %s" (show_lit x) (show_lit y)))
                    )
                ) 
            | Term     t -> (match ( l',  r') with
                    | (Number l'', Number r'') -> (match t with
                        | Add -> Ok (Number (Float.add l'' r''))
                        | Sub -> Ok (Number (Float.sub l'' r''))
                        )
                    | (String l'', String r'') -> (match t with
                        | Add -> Ok (String (String.cat l'' r''))
                        | _ ->   Error (Format.sprintf "invalid term for string cons? s")
                        )
                    | (x, y) -> 
                        Error ((Format.sprintf "invalid term operands: %s != %s" (show_lit x) (show_lit y)))
                )
            | Factor   f -> (match ( l',  r') with
                    | (Number l'', Number r'') -> (match f with
                        | Div -> Ok (Number (Float.div l'' r''))
                        | Mul -> Ok (Number (Float.mul l'' r''))
                        )
                    | (x, y) -> 
                        Error ((Format.sprintf "invalid term operands: %s != %s" (show_lit x) (show_lit y)))
                )
            | n -> Error ((Format.sprintf "unkown operation %s" (show_expr n)))
        )
    | e -> Error ((Format.sprintf "Unhandled  for expr: %s" (show_expr e)))
;;

let eval_exprs (Program el) =
    (Program (List.filter_map (fun x -> match x with 
        | (Raw (Eval (Literal Nil)))  ->  None
        | (Raw (Eval e'))  -> (match (eval e') with 
            |  Ok o    -> Some (Raw (Eval (Literal o))) 
            |  Error err -> 
                let _ = Format.printf "Raw Eval error!!: %s\n" err in
                None)
        | ((Side (Effect (Print e')))) -> (match (eval e') with 
            |  Ok o    -> let _ = (match o with
                    | (Bool b) -> 
                        Format.print_bool b 
                    | (Number n) -> 
                        Format.print_float n 
                    | (String s) -> 
                        Format.print_string s
                    | Nil -> 
                        Format.print_string "nil"
                ) in
                Some (Raw (Eval (Literal o))) 
            |  Error err -> 
                let _ = Format.printf "Effect Eval error!!: %s\n" err in
                None)          
    ) el))
;;
