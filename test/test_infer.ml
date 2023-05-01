open OUnit2
open Lib.Syntax
open Lib.Reconstruction

open For_test

let type_equal (PolyType (l, t)) s =
  let tbl = Hashtbl.create 10 in
  let rec aux a b =
    match a, b with
    | TInt, TInt | TBool, TBool -> true
    | TFun (s1, t1), TFun (s2, t2) ->
      aux s1 s2 && aux t1 t2
    | TVar x, TVar y ->
      if Hashtbl.mem tbl x then
        Hashtbl.find tbl x = y
      else
        (Hashtbl.add tbl x y; true)
    | _, _ -> false
  in
  if aux t s then
    List.filter (fun x -> not @@ Hashtbl.mem tbl x) l = []
  else
    false

let check_res env tm typ =
  try
    let res = reconstruct_toplevel env tm in
    match typ with
    | `Ok -> ()
    | `Content x -> assert_bool "content" @@ type_equal res x
    | _ -> assert_failure "Expected to fail but the type was inferred."
  with
  | Type_recursion _ -> assert_equal typ `Equi_rec
  | Type_mismatch _ -> assert_equal typ `Mismatch

let test_base _ =
  let open Infix in
  let chk = check_res Env.empty in
  chk (eint 114) @@ `Content TInt;
  chk (eint 514) @@ `Content TInt;
  chk (ebool true) @@ `Content TBool;
  chk (ebool false) @@ `Content TBool;
  chk (eint 114 - eint 200) @@ `Content TInt;
  chk (eint 114 < eint 514) @@ `Content TBool;
  chk (eint 1 * ebool true) @@ `Mismatch;
  chk (eint 114 = eint 114) @@ `Content TBool;
  chk (ebool true = ebool true) @@ `Mismatch

let epair = lfun ["s"; "t"; "f"] @@ lapply (var "f") [var "s"; var "t"]
let test_simple_poly _ =
  let chk = check_res Env.empty in
  chk (efun "x" @@ var "x") @@ `Content (TFun (TVar 0, TVar 0));
  chk epair @@ `Content (TFun (TVar 0, TFun (TVar 1, TFun (TFun (TVar 0, (TFun (TVar 1, TVar 2))), TVar 2))))

let test_let_poly _ =
  let chk = check_res Env.empty in
  chk (efun "s" @@ elet "f" (apply epair (var "s")) 
                @@ lapply epair [apply (var "f") (eint 1); apply (var "f") (ebool true)]) @@ `Ok

let test_rec _ =
  let chk = check_res Env.empty in
  chk (efix "f" @@ efun "x" @@ apply (var "f") (var "x")) @@ `Content (TFun (TVar 0, TVar 1));
  chk (efix "f" @@ efun "x" @@ var "f") @@ `Equi_rec

let suite =
  "type infer test" >::: [
    "test_base" >:: test_base;
    "test_simple_poly" >:: test_simple_poly;
    "test_let_poly" >:: test_let_poly;
    "test_rec" >:: test_rec
  ]
  
let () =
  run_test_tt_main suite