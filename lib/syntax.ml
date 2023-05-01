type ty =
  | TInt
  | TBool
  | TFun of ty * ty
  | TVar of int

type poly_ty = PolyType of int list * ty

type varname = string
module Env = Map.Make(String)


type binop =
  | Plus
  | Sub
  | Times
  | Div
  | Equal 
  | Less

type term' = 
  | Int of int
  | Bool of bool
  | Var of varname
  | Binop of binop * term * term
  | If of term * term * term
  | Fun of varname * term
  | Let of varname * term * term
  | Apply of term * term
  | Fix of varname * term
and term = { e : term'; pos : (Lexing.position * Lexing.position) option }
type value =
  | VInt of int
  | VBool of bool
  | VClosure of value Env.t * varname * term
  | VFix of value Env.t * varname * term * (Lexing.position * Lexing.position) option

type command =
  | TopTerm of term
  | TopDef of string * term

module For_test = struct
  let fill e = { e; pos = None }
  let eint x = fill (Int x)
  let ebool x = fill (Bool x)
  let var x = fill (Var x)
  let efun x t = fill (Fun (x, t))
  let lfun = List.fold_right efun
  let eif cond l r = fill (If (cond, l, r))
  let elet x a b = fill (Let (x, a, b))
  let apply f x = fill (Apply (f, x))
  let lapply = List.fold_left apply
  let efix s t = fill (Fix (s, t))

  module Infix = struct
    let ( + ) a b = fill (Binop (Plus, a, b))
    let ( - ) a b = fill (Binop (Sub, a, b))
    let ( * ) a b = fill (Binop (Times, a, b))
    let ( / ) a b = fill (Binop (Div, a, b))
    let ( = ) a b = fill (Binop (Equal, a, b))
    let ( < ) a b = fill (Binop (Less, a, b))
  end
end