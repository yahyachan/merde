open OUnit2
open Lib
open Syntax
open Interpret


let term_from_str s =
  let open Lib in
  match Parser.toplevel Lexer.token @@ Lexing.from_string @@ s ^ ";;" with
  | TopTerm t -> t
  | _ -> assert false

let run_str s =
  try
    let tm = term_from_str s in
    let typ = Reconstruction.reconstruct_toplevel Env.empty tm in
    let res = eval Env.empty tm in
    `Ok (typ, res)
  with
  | Parser.Error -> `Parser_error
  | Reconstruction.Type_mismatch (_, _) -> `Type_mismatch
  | Reconstruction.Type_recursion _ -> `Type_recursion
  | Var_not_found _ -> `Var_not_found

let res_eq a b =
  match a, b with
  | `Ok (t1, v1), `Ok (t2, v2) ->
    String.equal (string_of_polytype t1) (string_of_polytype t2) &&
    String.equal (string_of_value v1) (string_of_value v2)
  | _, _ -> a = b

let ztest a b = assert_equal ~cmp:res_eq (run_str a) b
let zstr a s = ztest a (run_str s)

let test_base _ =
  zstr "4" "3 + 1";
  zstr "18" "6 * 3";
  zstr "7" "21 /3";
  zstr "114" "115 * 2 - 116";
  zstr "true" "2 = 2";
  zstr "false" "2 = 3";
  zstr "true" "2 < 3";
  zstr "false" "3 < 3";
  ztest "1 * true" `Type_mismatch;
  ztest "true = true" `Type_mismatch

let test_if _ =
  zstr "if 2 < 2 then 1 else 2" "2";
  zstr "if 2 < 3 then 1 else 2" "1";
  ztest "if true then 1 else true" @@ `Type_mismatch

let fun_damn = VClosure (Env.empty, "s", For_test.rempty)

let test_let _ =
  zstr "let x = 3 in let y = 4 in let z = 5 in x * y - z" "7";
  zstr "let b = true in if b then 114 else 514" "114";
  ztest "let pair = fun x y f => f x y in fun s => let f = pair s in pair (f 1) (f true)" @@
    `Ok (PolyType ([1;16;19;13], TFun (TVar 1,
    TFun
     (TFun (TFun (TFun (TVar 1, TFun (TInt, TVar 16)), TVar 16),
       TFun (TFun (TFun (TVar 1, TFun (TBool, TVar 19)), TVar 19), TVar 13)),
     TVar 13))), fun_damn);
  ztest "let ev = 1 in b" @@ `Var_not_found

let test_rec _ =
  let rec factorial = function 0 -> 1 | n -> n * factorial (n - 1) in
  let base = "let fact = fix f => fun x => if x = 0 then 1 else x * f (x - 1) in fact " in
  for i = 0 to 10 do
    ztest (base ^ Int.to_string i) @@ `Ok (PolyType ([], TInt), VInt (factorial i))
  done;
  ztest "fix f => fun x => f" @@ `Type_recursion

let test_record _ =
  ztest "{   }" @@ `Ok (PolyType ([], TRecord TRowEmpty), VRecord Env.empty);
  let mid = "{ a = 1, c = 3, a = true, b = false, b = 2 }" in
  zstr "1" @@ mid ^ ".a";
  zstr "3" @@ mid ^ ".c";
  zstr "false" @@ mid ^ ".b";
  let mid = "{" ^ mid ^ " - a}" in
  zstr "true" @@ mid ^ ".a";
  let mid = "{a = 114 |" ^ mid ^ "}" in
  zstr "114" @@ mid ^ ".a";
  ztest "{}.x" `Type_mismatch;
  ztest "fun r => if true then {x=2 | r} else {y=2 | r}" `Type_mismatch;
  zstr ("let get_sec = fun r => let r = {r - a} in r.a in get_sec " ^ mid) "true"

let test_parser_fail _ =
  List.iter (fun x -> x `Parser_error) [
    ztest "";
    ztest "let x =1 in";
    ztest "a, b";
    ztest "()";
    ztest "fun a, b => b";
    ztest "{";
    ztest "{m - a";
    ztest "{a | m}"
  ]

let suite = "combine test" >::: [
  "test_base" >:: test_base;
  "test_if" >:: test_if;
  "test_let" >:: test_let;
  "test_rec" >:: test_rec;
  "test_record" >:: test_record;
  "test_parser_fail" >:: test_parser_fail
]

let _ = run_test_tt_main suite