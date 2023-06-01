open OUnit2
open Lib.Syntax
open Lib.Reconstruction

open For_test

let type_equal (PolyType (l, t)) s =
  let tbl = Hashtbl.create 10 in
  let rev = Hashtbl.create 10 in
  let rec aux a b =
    match a, b with
    | TInt, TInt | TBool, TBool | TRowEmpty, TRowEmpty -> true
    | TFun (s1, t1), TFun (s2, t2) ->
      aux s1 s2 && aux t1 t2
    | TVar x, TVar y ->
      if Hashtbl.mem tbl x then
        Hashtbl.find tbl x = y
      else
        (if Hashtbl.mem rev y 
          then false 
          else (Hashtbl.add tbl x y; Hashtbl.add rev y x; true))
    | TRecord r1, TRecord r2 | TVariants r1, TVariants r2 -> aux r1 r2
    | TRowExtension (m1, r1), TRowExtension (m2, r2) ->
      let xua _ l1 l2 = match l1, l2 with
        | None, None -> None
        | None, Some _ | Some _, None -> Some false
        | Some l1, Some l2 ->
          begin try Some (List.map2 aux l1 l2 |> List.fold_left (&&) true)
          with Invalid_argument _ -> Some false end
      in
      let mm = Env.merge xua m1 m2 in
      Env.fold (fun _ -> (&&)) mm true && aux r1 r2
    | _, _ -> false
  in
  let rec travel_tt = function
    | TFun (a, b) -> travel_tt a && travel_tt b
    | TVar x -> Hashtbl.mem rev x
    | TRecord r | TVariants r -> travel_tt r
    | TRowExtension (mp, r) ->
      Env.fold (fun _ l right -> right &&
        (List.map travel_tt l |> List.fold_left (&&) true)) mp true
      &&
      travel_tt r
    | _ -> true
  in
  if aux t s then
    List.filter (fun x -> not @@ Hashtbl.mem tbl x) l = []
    &&
    travel_tt s
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

let chk = check_res Env.empty
let chk_str s =
  let open Lib in
  let tm = Parser.toplevel Lexer.token @@ Lexing.from_string @@ s ^ ";;" in
  match tm with
  | TopTerm tm -> chk tm
  | _ -> assert false

let test_base _ =
  let open Infix in
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
  chk (efun "x" @@ var "x") @@ `Content (TFun (TVar 0, TVar 0));
  chk epair @@ `Content (TFun (TVar 0, TFun (TVar 1, TFun (TFun (TVar 0, (TFun (TVar 1, TVar 2))), TVar 2))))

let test_let_poly _ =
  chk (efun "s" @@ elet "f" (apply epair (var "s")) 
                @@ lapply epair [apply (var "f") (eint 1); apply (var "f") (ebool true)]) @@
  `Content (TFun (TVar 1,
  TFun
   (TFun (TFun (TFun (TVar 1, TFun (TInt, TVar 16)), TVar 16),
     TFun (TFun (TFun (TVar 1, TFun (TBool, TVar 19)), TVar 19), TVar 13)),
   TVar 13)))

let test_rec _ =
  chk (efix "f" @@ efun "x" @@ apply (var "f") (var "x")) @@ `Content (TFun (TVar 0, TVar 1));
  chk (efix "f" @@ efun "x" @@ var "f") @@ `Equi_rec

let test_record _ =
  chk rempty @@ `Content (TRecord TRowEmpty);
  chk_str "{}.x" `Mismatch;
  let get_sec = efun "r" @@ elet "r" (remove (var "r") "x") @@ select (var "r") "x" in
  chk get_sec @@
    `Content (TFun (TRecord (TRowExtension (Env.singleton "x" [TVar 114; TVar 514], TVar 1919)), TVar 514));
  let ill_fun = efun "r" @@ eif (ebool true) (ext (Env.singleton "x" [eint 2]) (var "r")) 
                                             (ext (Env.singleton "y" [eint 2]) (var "r")) in
  chk ill_fun `Mismatch

let test_variants _ =
  let ev_1 = (evariant "haha" @@ eint 4) in
  chk ev_1 @@ `Content (TVariants (TRowExtension (Env.singleton "haha" [TInt], TVar 114514)))

let suite =
  "type infer test" >::: [
    "test_base" >:: test_base;
    "test_simple_poly" >:: test_simple_poly;
    "test_let_poly" >:: test_let_poly;
    "test_rec" >:: test_rec;
    "test_record" >:: test_record;
    "test_variants" >:: test_variants
  ]
  
let () =
  run_test_tt_main suite