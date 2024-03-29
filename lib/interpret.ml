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
let get_record = function
  | VRecord m -> m
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
  | RecordEmpty -> VRecord Env.empty
  | RecordSelect (r, field) ->
    List.hd @@ Env.find field @@ get_record @@ eval env r
  | RecordExtension (ext, r) ->
    let ext = Env.map (fun l -> List.map (eval env) l) ext in
    let r = get_record @@ eval env r in
    VRecord (concat_env ext r)
  | RecordRestrict (r, field) ->
    let r = get_record @@ eval env r in
    VRecord (Env.add field (List.tl @@ Env.find field r) r)
  | Variant (name, t) -> VLabel (name, eval env t)
  | Match (tm, pls) ->
    eval_match env (eval env tm) pls
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
and eval_match env v = function
  | [] -> raise Runtime_type_error
  | (pt, res) :: xs -> 
    match try_binding pt v with
    | Some l ->
      eval (Env.add_seq (List.to_seq l) env) res
    | None -> eval_match env v xs
and try_binding pt v =
  match pt, v with
  | PVar name, _ -> Some [name, v]
  | PVariant (n1, np), VLabel (n2, nv) when String.equal n1 n2 ->
    try_binding np nv
  | _, _ -> None