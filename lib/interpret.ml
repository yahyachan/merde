open Syntax
exception Var_not_found of varname * (Lexing.position * Lexing.position) option
exception Runtime_type_error

let get_int = function
  | VInt x -> x
  | _ -> raise Runtime_type_error
let get_bool = function
  | VBool b -> b
  | _ -> raise Runtime_type_error
let get_closure = function
  | VClosure (a, b, c) -> (a, b, c)
  | _ -> raise Runtime_type_error


let rec eval env {e; pos; _} =
  match e with
  | Int x -> VInt x
  | Bool b -> VBool b
  | Var v -> begin
    match Env.find_opt v env with
    | Some (VFix (e, self, t, pos)) -> eval e @@ { e = Fix (self, t); pos }
    | Some x -> x
    | None -> raise @@ Var_not_found (v, pos)
  end
  | Fix (self, t) -> eval (Env.add self (VFix (env, self, t, pos)) env) t
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
