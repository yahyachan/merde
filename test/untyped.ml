open OUnit2
open Lib.Syntax
open Lib.Interpret

let fill e = { e; t = None; pos = None }
let eint x = fill (Int x)
let ebool x = fill (Bool x)
let var x = fill (Var x)
let efun x t = fill (Fun (x, t))
let eif cond l r = fill (If (cond, l, r))
let elet x a b = fill (Let (x, a, b))
let apply f x = fill (Apply (f, x))
let efix s t = fill (Fix (s, t))

module Infix = struct
  let ( + ) a b = fill (Binop (Plus, a, b))
  let ( - ) a b = fill (Binop (Sub, a, b))
  let ( * ) a b = fill (Binop (Times, a, b))
  let ( / ) a b = fill (Binop (Div, a, b))
  let ( = ) a b = fill (Binop (Equal, a, b))
  let ( < ) a b = fill (Binop (Less, a, b))
end

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

let suite =
  "untyped interpreter test" >::: [
    "test_arith" >:: test_arith;
    "test_if" >:: test_if;
    "test_let" >:: test_let;
    "test_fix" >:: test_fix
  ]


let () =
  run_test_tt_main suite