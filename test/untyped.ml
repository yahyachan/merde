open OUnit2
open Lib.Syntax
open Lib.Interpret

open For_test
let term_from_str s =
  let open Lib in
  match Parser.toplevel Lexer.token @@ Lexing.from_string @@ s ^ ";;" with
  | TopTerm t -> t
  | _ -> assert false

let ev = eval Env.empty

let test_arith _ =
  let open Infix in
  assert_equal (VInt 4) (ev (eint 3 + eint 1));
  assert_equal (VInt (-2)) (ev (eint 5 - eint 7));
  assert_equal (VInt 18) (ev (eint 6 * eint 3));
  assert_equal (VInt 7) (ev (eint 21 / eint 3));
  assert_equal (VBool true) (ev (eint 2 = eint 2));
  assert_equal (VBool false) (ev (eint 3 = eint 2));
  assert_equal (VBool true) (ev (eint 2 < eint 3));
  assert_equal (VBool false) (ev (eint 3 < eint 3))

let test_if _ =
  assert_equal (VInt 1) (ev @@ eif (ebool true) (eint 1) (eint 2));
  assert_equal (VInt 2) (ev @@ eif (ebool false) (eint 1) (eint 2))

let test_let _ =
  assert_equal (VInt 17) (ev @@ elet "x" (eint 4) @@
                                elet "y" (eint 5) @@
                                elet "z" (eint 3) @@
                                Infix.(var "x" * var "y" - var "z"));
  assert_equal (VInt 19) (ev @@ elet "b" (ebool false) @@ eif (var "b") (eint 17) (eint 19))

let zfix = efun "g" @@ apply (efun "x" @@ apply (var "x") (var "x")) @@
  efun "z" @@ apply (var "g") @@ efun "v" @@
  apply (apply (var "z") (var "z")) (var "v")

let factorial_z = apply zfix @@ efun "f" @@ efun "x" @@
  eif Infix.((var "x") < (eint 1)) (eint 1) @@
  Infix.(var "x" * apply (var "f") (var "x" - eint 1))

let factorial = efix "f" @@ efun "x" @@
  eif Infix.((var "x") < (eint 1)) (eint 1) @@
  Infix.(var "x" * apply (var "f") (var "x" - eint 1))

let rec fact = function
  | 0 -> 1
  | n -> n * fact (n - 1)

let test_fix _ = 
  for i = 0 to 10 do
    assert_equal (VInt (fact i)) (ev @@ apply factorial_z (eint i));
    assert_equal (VInt (fact i)) (ev @@ apply factorial (eint i))
  done

let test_record _ =
  let th = ["a", [eint 1; ebool true]; "c", [eint 3]; "b", [ebool false; eint 2]] in
  let th = th |> List.to_seq |> Env.of_seq |> erecord in
  assert_equal (VInt 1) (ev @@ select th "a");
  assert_equal (VBool false) (ev @@ select th "b");
  assert_equal (VInt 3) (ev @@ select th "c");
  let th = remove th "a" in
  assert_equal (VBool true) (ev @@ select th "a");
  let th = ext (Env.singleton "a" [eint 114]) th in
  assert_equal (VInt 114) (ev @@ select th "a")

let test_variants _ =
  let ev_1 = evariant "`haha" Infix.(eint 1 + eint 2 * eint 3 - eint 6) in
  assert_equal (VLabel ("`haha", VInt 1)) (ev ev_1);
  let env = Env.singleton "x" (ev ev_1) in
  let match_1 = term_from_str "match x with | `Ji u -> 2 | `haha x -> x end" in
  assert_equal (VInt 1) (eval env match_1)

let suite =
  "untyped interpreter test" >::: [
    "test_arith" >:: test_arith;
    "test_if" >:: test_if;
    "test_let" >:: test_let;
    "test_fix" >:: test_fix;
    "test_record" >:: test_record;
    "test_variants" >:: test_variants
  ]


let () =
  run_test_tt_main suite