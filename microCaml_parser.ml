open MicroCamlTypes
open Utils
open TokenTypes


(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None


let rec parse_expr toks =
  match toks with
  | Tok_Let::t -> parse_let t
  | Tok_If::t -> parse_if t
  | Tok_Fun::t -> parse_fun t
  | _  -> parse_or toks

(*let expr*)
  and parse_let toks = 
    let (rest_toks_after_rec, is_rec) = parse_rec toks in
    match parse_primary rest_toks_after_rec with
    | (rest_toks_after_var, var) ->
      let (rest_toks_after_eq, expr1) = parse_expr (match_token rest_toks_after_var Tok_Equal) in
      let (rest_toks_after_in, expr2) = parse_expr (match_token rest_toks_after_eq Tok_In) in
      let variable_name = match var with
        | ID(name) -> name
        | _ -> raise (InvalidInputException("Error in parsing, should be ID"))
      in
      (rest_toks_after_in, Let(variable_name, is_rec, expr1, expr2))
  (*rec expr*)
  and parse_rec toks = 
    match toks with
    | Tok_Rec::rest_toks -> (rest_toks, true)
    | _ -> (toks, false)

(*function expr*)
  and parse_fun toks = 
    match toks with
    | Tok_ID id :: remaining_toks ->
      let after_arw_toks, expr1 = parse_expr (match_token remaining_toks Tok_Arrow) in
      after_arw_toks, Fun (id, expr1)
    | _ -> raise (InvalidInputException("err in parsing: 'tok_id' token unavailable"))


(* if expr *)
  and parse_if toks =
    let (rest1, condition) = parse_expr toks in
    let rest2 = match_token rest1 Tok_Then in

    let (rest3, then_expr) = parse_expr rest2 in
    let rest4 = match_token rest3 Tok_Else in

    let (rest5, else_expr) = parse_expr rest4 in
    (rest5, If(condition, then_expr, else_expr))

(*or expr*)
  and parse_or toks = 
    let (rest1, expr1) = parse_and toks in
    match rest1 with
    | Tok_Or::t -> let (rest2, expr2) = parse_or t in
                   rest2, Binop(Or, expr1, expr2)
    | _ -> rest1, expr1

(*and expr*)
  and parse_and toks = 
    let (remaining_toks_after_e1, e1) = parse_eq toks in
    match remaining_toks_after_e1 with
    | Tok_And::t -> 
        let (remaining_toks_after_e2, e2) = parse_and t in 
        (remaining_toks_after_e2, Binop(And, e1, e2))
    | _ -> (remaining_toks_after_e1, e1)

(*equality expr*)
  and parse_eq toks = 
    let (remaining_toks_after_rel, rel_expr) = parse_rel toks in
    match remaining_toks_after_rel with
    | Tok_Equal::t -> 
        let (remaining_toks_after_eq, eq_expr) = parse_eq t in 
        (remaining_toks_after_eq, Binop(Equal, rel_expr, eq_expr))
    | Tok_NotEqual::t -> 
        let (remaining_toks_after_neq, neq_expr) = parse_eq t in 
        (remaining_toks_after_neq, Binop(NotEqual, rel_expr, neq_expr))
    | _ -> (remaining_toks_after_rel, rel_expr)


(*relational exp*)
  and parse_rel toks = 
    let (remaining_toks_after_add, add_expr) = 
      parse_add toks in
      match remaining_toks_after_add with
      | Tok_Less::t -> 
        let (remaining_toks_after_rel, rel_expr) = 
        parse_rel t in (remaining_toks_after_rel, Binop(Less, add_expr, rel_expr))

      | Tok_Greater::t -> 
        let (remaining_toks_after_rel, rel_expr) = 
        parse_rel t in (remaining_toks_after_rel, Binop(Greater, add_expr, rel_expr))
      
      | Tok_LessEqual::t -> 
        let (remaining_toks_after_rel, rel_expr) = 
        parse_rel t in (remaining_toks_after_rel, Binop(LessEqual, add_expr, rel_expr))
      
      | Tok_GreaterEqual::t -> 
        let (remaining_toks_after_rel, rel_expr) = 
        parse_rel t in (remaining_toks_after_rel, Binop(GreaterEqual, add_expr, rel_expr))
      
      | _ -> (remaining_toks_after_add, add_expr)

(*additive expr*)
  and parse_add toks = 

  let (remaining_toks_after_mult, mult_expr) =
  parse_mult toks in
  match remaining_toks_after_mult with
  | Tok_Add::t -> let (remaining_toks_after_add, add_expr) = 
                  parse_add t in 
                  (remaining_toks_after_add, Binop(Add, mult_expr, add_expr))
  
  | Tok_Sub::t -> let (remaining_toks_after_sub, sub_expr) = 
                  parse_add t in 
                  (remaining_toks_after_sub, Binop(Sub, mult_expr, sub_expr))
  
  | _ -> (remaining_toks_after_mult, mult_expr)


(*multiplicative expr*)
  and parse_mult toks = 
  let (remaining_toks_after_e1, expr1) = 
    parse_concat toks in
    match remaining_toks_after_e1 with
    
    | Tok_Mult::t -> 
      let (remaining_toks_after_e2, expr2) = 
      parse_mult t in 
      (remaining_toks_after_e2, Binop(Mult, expr1, expr2))
    
    | Tok_Div::t -> 
      let (remaining_toks_after_e2, expr2) = 
      parse_mult t in 
      (remaining_toks_after_e2, Binop(Div, expr1, expr2))
    
    | _ -> (remaining_toks_after_e1, expr1)

(*concat expr*)
  and parse_concat toks = 
    let (remaining_toks_after_e1, expr1) = 
    (parse_unary toks) in
    match remaining_toks_after_e1 with

    | Tok_Concat::t -> 
      let (remaining_toks_after_e2, expr2) = 
      (parse_concat t) in (remaining_toks_after_e2, Binop(Concat, expr1, expr2))
    
    | _ -> (remaining_toks_after_e1, expr1)

(*unary expr*)
  and parse_unary toks = 
    match toks with
    
    | Tok_Not::t -> 
      
      let (remaining_toks_after_e1, expr1) = 
      (parse_unary t) in 
      (remaining_toks_after_e1, Not(expr1))
    
    | _ -> (parse_fcall toks)

(*function call expr*)
  and parse_fcall toks = 
    let (remaining_toks_after_e1, expr1) = 
    parse_primary toks in

    (* Check next tokens for function calls *)
    match lookahead remaining_toks_after_e1 with

    | Some (Tok_Int _)        (* integer *)
    | Some (Tok_Bool _)       (* boolean literal *)
    | Some (Tok_String _)     (* str literal *)
    | Some (Tok_ID _)         (* var identifier *)
    | Some Tok_LParen         (* nested function call *)

      ->
      let (remaining_toks_after_e2, expr2) =
        parse_primary remaining_toks_after_e1 in
        (remaining_toks_after_e2, FunctionCall(expr1, expr2))

    (* If no call return parsed expr *)
    | _ -> (remaining_toks_after_e1, expr1)

(*primary expr*)
  and parse_primary toks = 
    match toks with
    | Tok_Int(v)::t -> (t, Value(Int(v)))
    | Tok_Bool(v)::t -> (t, Value(Bool(v)))
    | Tok_String(v)::t -> (t, Value(String(v)))
    | Tok_ID(v)::t -> (t, ID(v))

    | Tok_LParen::t ->  let (remaining_toks_after_e1, expr1) = (parse_expr t) in
                        (match remaining_toks_after_e1 with
                        | Tok_RParen::t -> (t, expr1)
                        | _ -> raise (InvalidInputException("err in parsing: left paranthesis not found")))
    | _ -> raise (InvalidInputException("err in parsing: invalid input"))

let rec parse_mutop toks = 
  match toks with
  | Tok_Def :: t -> 
      let rest1, id_token = 
        match t with
        | Tok_ID id :: ts -> ts, ID id
        | _ -> raise (InvalidInputException("mutop: ID error"))
      in
      let rest2 = match_token rest1 Tok_Equal in
      let rest3, expr = parse_expr rest2 in
      let rest4 = match_token rest3 Tok_DoubleSemi in
      let id =
        match id_token with
        | ID x -> x
        | _ -> raise (InvalidInputException("mutop: ID error"))
      in
      rest4, Def (id, expr)

  | Tok_DoubleSemi :: t -> t, NoOp
  | _ -> 
      let rest, expr = parse_expr toks in
      match_token rest Tok_DoubleSemi, Expr expr