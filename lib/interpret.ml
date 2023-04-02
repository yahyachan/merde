open Syntax

let get_int = function
  | VInt x -> x
  | _ -> assert false
let get_bool = function
  | VBool b -> b
  | _ -> assert false
let get_closure = function
  | VClosure (a, b, c) -> (a, b, c)
  | _ -> assert false


let rec eval env {e; _} =
  match e with
  | Int x -> VInt x
  | Bool b -> VBool b
  | Var v -> Env.find v env
  | Fun (x, t) -> VClosure (env, x, t)
  | Let (x, a, b) ->
    eval (Env.add x (eval env a) env) b
  | Apply (f, v) ->
    let (nenv, vn, body) = get_closure (eval env f) in
    let rv = eval env v in
    eval (Env.add vn rv nenv) body
  | If (cond, l, r) ->
    eval env (if get_bool (eval env cond) then l else r)
  | Binop (mid, l, r) ->
    let a = eval env l in
    let b = eval env r in
    match mid with
    | Plus -> VInt (get_int a + get_int b)
    | Sub -> VInt (get_int a - get_int b)
    | Times -> VInt (get_int a * get_int b)
    | Div -> VInt (get_int a / get_int b)
    | Equal -> VBool (a = b)
    | Less -> VBool (get_int a < get_int b)
