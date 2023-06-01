module Env = Map.Make(String)
type ty =
  | TInt
  | TBool
  | TFun of ty * ty
  | TVar of int
  | TRowEmpty
  | TRowExtension of (ty list) Env.t * row
  | TRecord of row
and row = ty

let rec find_row_tail = function
  | TRowEmpty -> None
  | TVar x -> Some x
  | TRowExtension (_, rest) -> find_row_tail rest
  | _ -> assert false
let insert_lenv env k v =
  match Env.find_opt k env with
  | None -> Env.add k [v] env
  | Some xs -> Env.add k (v :: xs) env

type poly_ty = PolyType of int list * ty

type varname = string

let concat_env m1 m2 =
  let aux _ l1 l2 =
    match l1, l2 with
    | None, None -> None
    | Some l, None | None, Some l -> Some l
    | Some l, Some r -> Some (l @ r)
  in
  Env.merge aux m1 m2

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
  | RecordSelect of term * varname
  | RecordExtension of (term list) Env.t * term
  | RecordRestrict of term * varname
  | RecordEmpty
and term = { e : term'; pos : (Lexing.position * Lexing.position) option }
type value =
  | VInt of int
  | VBool of bool
  | VClosure of value Env.t * varname * term
  | VFix of value Env.t * varname * term * (Lexing.position * Lexing.position) option
  | VRecord of (value list) Env.t

type command =
  | TopTerm of term
  | TopDef of string * term
  | TopExit

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

  let select r field = fill @@ RecordSelect (r, field)
  let rempty = fill RecordEmpty
  let ext m r = fill @@ RecordExtension (m, r)
  let erecord m = ext m rempty
  let remove r field = fill @@ RecordRestrict (r, field)

  module Infix = struct
    let ( + ) a b = fill (Binop (Plus, a, b))
    let ( - ) a b = fill (Binop (Sub, a, b))
    let ( * ) a b = fill (Binop (Times, a, b))
    let ( / ) a b = fill (Binop (Div, a, b))
    let ( = ) a b = fill (Binop (Equal, a, b))
    let ( < ) a b = fill (Binop (Less, a, b))
  end
end


let rec string_of_value = function
  | VInt x -> string_of_int x
  | VBool b -> string_of_bool b
  | VRecord r ->
    let buf = Buffer.create 15 in
    let lk = ref false in
    let aux s v =
      if !lk then
        Buffer.add_string buf ", "
      else
        lk := true;
      Buffer.add_string buf s;
      Buffer.add_string buf " = ";
      Buffer.add_string buf @@ string_of_value v
    in
    Buffer.add_char buf '{';
    Env.iter (fun s l -> List.iter (aux s) l) r;
    Buffer.add_char buf '}';
    String.of_bytes @@ Buffer.to_bytes buf
  | _ -> "<fun>"

let literal_stream () =
  let f x =
    if x < 26 then
      let c = Char.chr (97 + x) in 
      Some ("'" ^ String.make 1 c)
    else
      Some ("'" ^ Int.to_string x)
  in
  Stream.from f

let is_complex_type = function
  | TFun _ -> true
  | TRowExtension _ -> true
  | _ -> false
let string_of_polytype (PolyType (l, t)) =
  let buf = Buffer.create 16 in
  let stream = literal_stream () in
  let tbl = Hashtbl.create 10 in
  List.iter (fun x -> Hashtbl.add tbl x @@ Stream.next stream) l;
  let rec aux = function
    | TInt -> Buffer.add_string buf "int"
    | TBool -> Buffer.add_string buf "bool"
    | TVar x ->
      let th = if Hashtbl.mem tbl x then Hashtbl.find tbl x else "'_" ^ Int.to_string x in
      Buffer.add_string buf th
    | TFun (s, t) ->
      let b = is_complex_type s in
      if b then Buffer.add_char buf '(' else ();
      aux s;
      if b then Buffer.add_char buf ')' else ();
      Buffer.add_string buf " -> "; 
      aux t
    | TRowEmpty -> ()
    | TRowExtension (mp, rest) ->
      Env.iter (fun s l -> List.iter 
                           (fun ty -> Buffer.add_string buf @@ s ^ " : ";
                            aux ty; Buffer.add_string buf "; ") l) mp;
      aux rest
    | TRecord r ->
      Buffer.add_char buf '{';
      aux r;
      Buffer.add_char buf '}'
  in
  aux t;
  String.of_bytes @@ Buffer.to_bytes buf
let string_of_type t = string_of_polytype @@ PolyType ([], t)