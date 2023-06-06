open OUnit2
open Lib.Lexer
open Lib.Parser

let continious_lex str =
  let buf = Lexing.from_string str in
  let rec aux acc = 
    match token buf with
    | EOF -> List.rev acc
    | tk -> aux (tk :: acc)
  in
  try `Ok (aux [])
  with Failure _ -> `Fail

let test_empty _ =
  assert_equal (continious_lex "") @@ `Ok [];
  assert_equal (continious_lex "  \t\n\n\t\r\n\r") @@ `Ok []

let test_ok _ =
  assert_equal (continious_lex "())in,let_ _1Ma->==") @@
    `Ok [LPAREN; RPAREN; RPAREN; IN; COMMA; VARIABLE "let_"; VARIABLE "_1Ma"; SARROW; EQUAL; EQUAL]

let test_fail _ =
  assert_equal (continious_lex ";") @@ `Fail

let suite =
  "lexer test" >::: [
    "test_empty" >:: test_empty;
    "test_ok" >:: test_ok;
    "test_fail" >:: test_fail
  ]

let _ = run_test_tt_main suite