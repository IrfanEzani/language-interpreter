open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 


let ref_extend env x v = (x, ref v)::env
let extend env x v = (x,v)::env

let rec ref_lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else ref_lookup t x

let rec lookup env x = 
  match env with
  [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then value else lookup t x

let ref_extend_tmp env x = (x, ref (Int 0))::env
let rec ref_update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else ref_update t x v
        
let rec remove env x = match env with
  [] -> []
  | (var,value)::t -> if x = var then t else (var,value)::(remove t x)


(****************************************************************)
(* Part 1: Evaluating expressions *)
(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)

(*** helper functions ***)
let int_check = function
  | Int x -> x
  | _ -> raise (TypeError "Type error: not an int")

let bool_check = function
  | Bool x -> x
  | _ -> raise (TypeError "Type error: not a bool")

let str_check = function
  | String x -> x
  | _ -> raise (TypeError "Type error: not a string")


let compatible v1 v2 = match v1, v2 with
  | Int _, Int _ -> true
  | Bool _, Bool _ -> true
  | String _, String _ -> true
  | _ -> raise (TypeError "Type error: not compatible")

(** eval_expr function **)
let rec eval_expr env = function

  | Value v -> v
  | ID id -> ref_lookup env id
  | Not x ->
      let value = eval_expr env x in
      (match value with
      | Bool b -> Bool (not b)
      | _ -> raise (TypeError "Type error: not a bool"))

  | Binop(o, x1, x2) -> 
      (let v1 = eval_expr env x1 in
        let v2 = eval_expr env x2 in
        
        match o with
        (*arithematic ops*)
          | Add -> Int(int_check v1 + int_check v2)
          | Sub -> Int(int_check v1 - int_check v2)
          | Mult -> Int(int_check v1 * int_check v2)
          | Div -> if int_check v2 = 0 then raise DivByZeroError else Int(int_check v1 / int_check v2)
        (*comparison ops*)
          | Greater -> Bool(int_check v1 > int_check v2)
          | GreaterEqual -> Bool(int_check v1 >= int_check v2)
          | Less -> Bool(int_check v1 < int_check v2)
          | LessEqual -> Bool(int_check v1 <= int_check v2)
        (*str concat*)
          | Concat -> String(str_check v1 ^ str_check v2)
        (*equality ops*)
          | Equal | NotEqual -> 
          
          (let compatible_types = compatible v1 v2 in
            let are_equal = v1 = v2 in
            match (compatible_types, are_equal) with
            | (true, true) -> Bool(if o = Equal then true else false)
            | (true, false) -> Bool(if o = Equal then false else true)
            | _ -> raise (TypeError "Type error: not compatible"))

        (*boolean ops*)
          | Or -> Bool(bool_check v1 || bool_check v2)
          | And -> Bool(bool_check v1 && bool_check v2))

  | If(guard, tb, fb) ->  if eval_expr env guard = Bool true then eval_expr env tb else eval_expr env fb
  | Let(var, stat, init, body) ->
      let new_env =
        if stat then
          let new_env = ref_extend_tmp env var in
          ref_update new_env var (eval_expr new_env init);
          new_env
        else
          ref_extend env var (eval_expr env init)
      in eval_expr new_env body


  | Fun(param, body) -> Closure(env, param, body)
  | FunctionCall(f1, f2) -> 
      match eval_expr env f1 with
      
      | Closure(a, x, e) ->
          let v = eval_expr env f2 in
          let new_env = ref_extend a x v in
          eval_expr new_env e
      
      | _ -> raise (TypeError "Type error: not a function")


(****************************************************************)
(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m =
  match m with

  | Def(id, expr) ->
    let value, new_env = 
      let tmp = ref_extend_tmp env id in
      let v = eval_expr tmp expr in
      ref_update tmp id v;
      v, tmp
    in (new_env, Some value)

  | Expr(expr) ->
    let v = eval_expr env expr in
    (env, Some v)

  | NoOp ->
    (env, None)

