open Syntax

let new_stream () = Stream.from @@ fun x -> Some x

module Typevar_env = Map.Make(Int)
let rec type_substitute env = function
  | TVar x when Typevar_env.mem x env -> TVar (Typevar_env.find x env)
  | TFun (s, t) -> TFun (type_substitute env s, type_substitute env t)
  | t -> t

let instantiate level uf (PolyType (vars, t)) =
  match vars with
  | [] -> t
  | _ ->
    let assoc = List.map (fun u -> u, Utils.Unionfind.new_var uf (fun x -> `TVar (level, x))) vars in
    let env = Typevar_env.of_seq @@ List.to_seq assoc in 
    type_substitute env t

let side_typeof = function
  | Less | Equal -> TInt, TBool
  | _ -> TInt, TInt

let rec check_occur uf tv = function
  | TInt | TBool -> false
  | TFun (s, t) -> check_occur uf tv s || check_occur uf tv t
  | TVar x -> Utils.Unionfind.is_same uf x tv

exception Type_recursion of int * ty
exception Type_mismatch of ty * ty
let rec unify uf a b =
  let open Utils.Unionfind in
  match a, b with
  | TInt, TInt | TBool, TBool -> ()
  | TFun (s1, t1), TFun (s2, t2) ->
    unify uf s1 s2; unify uf t1 t2
  | TVar x, TVar y -> begin
    match get uf x, get uf y with
    | `TVar (ls, s), `TVar (lt, t) ->
      let minorant = if ls < lt then s else t in
      merge_set uf s t;
      set uf s @@ `TVar (Int.min ls lt, minorant)
    | `Cont s, `Cont t -> unify uf s t
    | `Cont s, `TVar (_, v) | `TVar (_, v), `Cont s -> unify uf s (TVar v)
  end
  | TVar x, t | t, TVar x ->
    if check_occur uf x t then raise @@ Type_recursion (x, t) else try_set uf x t
  | a, b -> raise @@ Type_mismatch (a, b)
and try_set uf pos t =
  let open Utils.Unionfind in
  match get uf pos with
  | `TVar _ -> set uf pos @@ `Cont t
  | `Cont u -> unify uf u t

let rec travel uf tbl lp = function
  | TFun (s, t) -> TFun (travel uf tbl lp s, travel uf tbl lp t)
  | TVar x -> begin
    match Utils.Unionfind.get uf x with
    | `TVar (s, v) -> if s >= lp then Hashtbl.replace tbl v () else (); TVar v
    | `Cont t -> travel uf tbl lp t
  end
  | prim -> prim

let solve lp uf t constraints =
  List.iter (fun (a, b) -> unify uf a b) constraints;
  let tbl = Hashtbl.create 10 in
  let res_typ = travel uf tbl lp t in
  PolyType (List.of_seq @@ Hashtbl.to_seq_keys tbl, res_typ)

let rec reconstruct lp uf env tm =
  let t, con = to_constriants lp uf env tm in
  solve lp uf t con
and to_constriants lp uf env tm = 
  match tm.e with
  | Int _ -> TInt, []
  | Bool _ -> TBool, []
  | Var s -> instantiate lp uf @@ Env.find s env, []
  | Binop (op, l, r) ->
    let inp_type, out_type = side_typeof op in
    let l, l_con = to_constriants lp uf env l in
    let r, r_con = to_constriants lp uf env r in
    out_type, (l, inp_type) :: (r, inp_type) :: List.append l_con r_con
  | If (b, l, r) ->
    let b, b_con = to_constriants lp uf env b in
    let l, l_con = to_constriants lp uf env l in
    let r, r_con = to_constriants lp uf env r in
    l, (b, TBool) :: (l, r) :: (b_con @ l_con @ r_con)
  | Fun (x, t) ->
    let nv = TVar (Utils.Unionfind.new_var uf (fun x -> `TVar (lp, x))) in
    let res, con = to_constriants lp uf (Env.add x (PolyType ([], nv)) env) t in
    TFun (nv, res), con
  | Let (x, s, t) ->
    let x_typ = reconstruct (lp + 1) uf env s in
    to_constriants lp uf (Env.add x x_typ env) t
  | Apply (s, t) ->
    let res = TVar (Utils.Unionfind.new_var uf (fun x -> `TVar (lp, x))) in
    let t1, c1 = to_constriants lp uf env s in 
    let t2, c2 = to_constriants lp uf env t in
    res, (t1, TFun (t2, res)) :: (c1 @ c2)
  | Fix (x, t) ->
    let self_typ = TVar (Utils.Unionfind.new_var uf (fun x -> `TVar (lp, x))) in
    let res, con = to_constriants lp uf (Env.add x (PolyType ([], self_typ)) env) t in
    res, (res, self_typ) :: con

let reconstruct_toplevel env t = reconstruct 0 (Utils.Unionfind.init 1 (fun x -> `TVar (0,x))) env t