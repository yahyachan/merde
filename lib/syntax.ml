type ty =
  | TInt
  | TBool
  | TFun of ty * ty


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
and term = { e : term'; mutable t : ty option; pos : (Lexing.position * Lexing.position) option }
type value =
  | VInt of int
  | VBool of bool
  | VClosure of value Env.t * varname * term

type command =
  | TopTerm of term
  | TopDef of string * term