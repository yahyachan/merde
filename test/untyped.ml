open OUnit2
open Lib.Untyped
open Lib.Interpret

let fill e = { e; t = None }
let eint x = fill (Int x)
let ebool x = fill (Bool x)
let var x = fill (Var x)
let efun x t = fill (Fun (x, t))
let eif cond l r = fill (If (cond, l, r))
let apply f x = fill (Apply (f, x))

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

let zfix = efun "g" @@ apply (efun "x" @@ apply (var "x") (var "x")) @@
  efun "z" @@ apply (var "g") @@ efun "v" @@
  apply (apply (var "z") (var "z")) (var "v")

let factorial = apply zfix @@ efun "f" @@ efun "x" @@
  eif Infix.((var "x") < (eint 1)) (eint 1) @@
  Infix.(var "x" * apply (var "f") (var "x" - eint 1))

let test_zfix _ = assert_equal (VInt 3628800) (ev @@ apply factorial (eint 10))

let suite =
  "untyped interpreter test" >::: [
    "test_arith" >:: test_arith;
    "test_if" >:: test_if;
    "test_zfix" >:: test_zfix
  ]


let () =
  run_test_tt_main suite