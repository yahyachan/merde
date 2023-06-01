open Syntax

let new_stream () = Stream.from @@ fun x -> Some x

module Typevar_env = Map.Make(Int)
let rec type_substitute env = function
  | TVar x when Typevar_env.mem x env -> TVar (Typevar_env.find x env)
  | TFun (s, t) -> TFun (type_substitute env s, type_substitute env t)
  | TRecord r -> TRecord (type_substitute env r)
  | TRowExtension (mp, tail) ->
    let mp = Env.map (List.map (type_substitute env)) mp in
    begin
      match type_substitute env tail with
      | TRowExtension (m2, tail) ->
        TRowExtension (concat_env mp m2, tail)
      | t -> TRowExtension (mp, t)
    end
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
  | TRowEmpty -> false
  | TRecord s -> check_occur uf tv s
  | TRowExtension (mp, rest) ->
    let rec chk_list = function
      | [] -> true
      | x :: xs -> check_occur uf tv x && chk_list xs
    in
    Env.fold (fun _ cont right -> chk_list cont && right) mp true
    &&
    check_occur uf tv rest


exception Type_recursion of int * ty
exception Type_mismatch of ty * ty

let new_var uf lp =
  let ret = (Utils.Unionfind.new_var uf (fun x -> `TVar (lp, x))) in
  TVar ret
let env_remove_empty_list m =
  let aux _ l1 l2 =
    match l1, l2 with
    | None, _ | Some [], _ -> None
    | Some l, _ -> Some l
  in
  Env.merge aux m Env.empty

let rec get_min_level uf = function
  | TVar v -> begin
    match Utils.Unionfind.get uf v with
    | `TVar (s, _) -> s
    | `Cont t -> get_min_level uf t
  end
  | TFun (s, t) -> Int.min (get_min_level uf s) (get_min_level uf t)
  | TRecord r -> get_min_level uf r
  | TRowExtension (mp, tail) ->
    Int.min (get_min_level uf tail) @@
    Env.fold 
      (fun _ l right -> Int.min right @@
        List.fold_right Int.min (List.map (get_min_level uf) l) Int.max_int) mp Int.max_int
  | _ -> Int.max_int
let rec restrcit_level uf level = function
  | TVar v -> begin
    match Utils.Unionfind.get uf v with
    | `TVar (s, k) -> Utils.Unionfind.set uf v @@ `TVar (Int.min s level, k)
    | `Cont t -> restrcit_level uf level t
  end
  | TFun (s, t) -> restrcit_level uf level s; restrcit_level uf level t
  | TRecord r -> restrcit_level uf level r
  | TRowExtension (mp, tail) ->
    Env.iter (fun _ -> List.iter (restrcit_level uf level)) mp;
    restrcit_level uf level tail
  | _ -> ()

let rec row_unify lb uf (m1, r1) (m2, r2) =
  let rec aux label l1 l2 =
    match l1, l2 with
    | None, None
    | Some [], Some [] | Some [], None | None, Some [] -> None
    | Some l, None | Some l, Some [] -> Some ([], l)
    | None, Some l | Some [], Some l -> Some (l, [])
    | Some (x :: xs), Some (y :: ys) ->
      unify lb uf x y; aux label (Some xs) (Some ys)
  in
  let rr = Env.merge aux m1 m2 in
  let missing1 = Env.map fst rr |> env_remove_empty_list
  and missing2 = Env.map snd rr |> env_remove_empty_list in
  match Env.is_empty missing1, Env.is_empty missing2 with
  | true, true -> unify lb uf r1 r2
  | true, false -> unify lb uf r2 @@ TRowExtension (missing2, r1)
  | false, true -> unify lb uf r1 @@ TRowExtension (missing1, r2)
  | false, false -> begin
    let nv = new_var uf lb in
    match r1 with
    | TRowEmpty -> unify lb uf TRowEmpty @@ TRowExtension (missing1, nv)
    | TVar _ -> unify lb uf r2 @@ TRowExtension (missing2, nv);
                unify lb uf r1 @@ TRowExtension (missing1, nv)
    | _ -> assert false
  end
and unify lb uf a b =
  let open Utils.Unionfind in
  match a, b with
  | TInt, TInt | TBool, TBool | TRowEmpty, TRowEmpty -> ()
  | TRecord r1, TRecord r2 -> unify lb uf r1 r2
  | TFun (s1, t1), TFun (s2, t2) ->
    unify lb uf s1 s2; unify lb uf t1 t2
  | TVar x, TVar y -> begin
    match get uf x, get uf y with
    | `TVar (ls, s), `TVar (lt, t) ->
      let minorant = if ls < lt then s else t in
      merge_set uf s t;
      set uf s @@ `TVar (Int.min ls lt, minorant)
    | `Cont s, `Cont t -> unify lb uf s t
    | `Cont s, `TVar (_, v) | `TVar (_, v), `Cont s -> unify lb uf s (TVar v)
  end
  | TVar x, t | t, TVar x ->
    if check_occur uf x t then raise @@ Type_recursion (x, t) else try_set lb uf x t
  | TRowExtension (m1, r1), TRowExtension (m2, r2)
    when find_row_tail r1 = None || find_row_tail r1 <> find_row_tail r2 ->
    row_unify lb uf (m1, r1) (m2, r2)
  | a, b -> raise @@ Type_mismatch (a, b)
and try_set lb uf pos t =
  let open Utils.Unionfind in
  match get uf pos with
  | `TVar (s, _) ->
    restrcit_level uf s t;
    set uf pos @@ `Cont t
  | `Cont u -> unify lb uf u t

let rec travel uf tbl lp = function
  | TFun (s, t) -> TFun (travel uf tbl lp s, travel uf tbl lp t)
  | TVar x -> begin
    match Utils.Unionfind.get uf x with
    | `TVar (s, v) -> if s >= lp then Hashtbl.replace tbl v () else (); TVar v
    | `Cont t -> travel uf tbl lp t
  end
  | TRowExtension (ext, tail) ->
    let ext = Env.map (List.map (travel uf tbl lp)) ext in
    let tail = travel uf tbl lp tail in begin
      match tail with
      | TRowExtension (e2, t) ->
        TRowExtension (concat_env ext e2, t)
      | t -> TRowExtension (ext, t)
    end
  | TRecord r ->
    TRecord (travel uf tbl lp r)
  | prim -> prim

let rec infer lp uf env tm =
  let res = infer_inner lp uf env tm in
  let tbl = Hashtbl.create 10 in
  let res = travel uf tbl lp res in
  PolyType (List.of_seq @@ Hashtbl.to_seq_keys tbl, res)
and infer_inner lp uf env tm =
  match tm.e with
  | Int _ -> TInt
  | Bool _ -> TBool
  | Var s -> instantiate lp uf @@ Env.find s env
  | Binop (op, l, r) ->
    let inp_type, out_type = side_typeof op in
    unify lp uf inp_type (infer_inner lp uf env l);
    unify lp uf inp_type (infer_inner lp uf env r);
    out_type
  | If (b, l, r) ->
    unify lp uf TBool (infer_inner lp uf env b);
    let ret = (infer_inner lp uf env l) in
    unify lp uf ret (infer_inner lp uf env r);
    ret
  | Fun (x, t) ->
    let nv = new_var uf lp in
    let res = infer_inner lp uf (Env.add x (PolyType ([], nv)) env) t in
    TFun (nv, res)
  | Let (x, s, t) ->
    let x_typ = infer (lp + 1) uf env s in
    infer_inner lp uf (Env.add x x_typ env) t
  | Apply (s, t) ->
    let res = new_var uf lp in
    let t1 = infer_inner lp uf env s
    and t2 = infer_inner lp uf env t in
    unify lp uf t1 @@ TFun (t2, res);
    res
  | Fix (x, t) ->
    let self_typ = new_var uf lp in
    let res = infer_inner lp uf (Env.add x (PolyType ([], self_typ)) env) t in
    unify lp uf res self_typ;
    res
  | RecordEmpty -> TRecord TRowEmpty
  | RecordSelect (r, field) ->
    let row_var = new_var uf lp in
    let ret_var = new_var uf lp in
    let res = TRecord (TRowExtension (Env.singleton field [ret_var], row_var)) in
    unify lp uf res (infer_inner lp uf env r);
    ret_var
  | RecordExtension (m, r) ->
    let m = Env.map (List.map (infer_inner lp uf env)) m in
    let row_var = new_var uf lp in
    unify lp uf (TRecord row_var) (infer_inner lp uf env r);
    TRecord (TRowExtension (m, row_var))
  | RecordRestrict (r, field) ->
    let row_var = new_var uf lp in
    let ret_var = new_var uf lp in
    let mid = TRecord (TRowExtension (Env.singleton field [ret_var], row_var)) in
    unify lp uf mid (infer_inner lp uf env r);
    TRecord row_var


let reconstruct_toplevel env t = infer 0 (Utils.Unionfind.init 1 (fun x -> `TVar (0, x))) env t