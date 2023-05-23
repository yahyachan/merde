open Lib
open Syntax

let top_env = ref (Env.empty)
let top_type_env = ref (Env.empty)
let string_of_value = function
  | VInt x -> string_of_int x
  | VBool b -> string_of_bool b
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
  in
  aux t;
  String.of_bytes @@ Buffer.to_bytes buf
let string_of_type t = string_of_polytype @@ PolyType ([], t)

let read_command () =
  let buf = Buffer.create 16 in
  let rec aux last_char =
    let c = input_char stdin in
    Buffer.add_char buf c;
    if c = ';' && last_char = ';' then
      ()
    else
      aux c
  in
  aux '0';
  String.of_bytes (Buffer.to_bytes buf)

let loop () =
  let open Interpret in
  let parser = Parser.toplevel Lexer.token in
  let tyck_eval tm =
    let t = Reconstruction.reconstruct_toplevel !top_type_env tm in
    let v = eval !top_env tm in 
    t, v
  in
  while true do
    try
      print_string "\nmerde> ";
      flush stdout;
      let buf = Lexing.from_string (read_command ()) in
      let name, (t, v) = begin
        match parser buf with
        | TopTerm t -> "-", tyck_eval t
        | TopDef (s, t) ->
          let t, v = tyck_eval t in
          top_type_env := Env.add s t !top_type_env;
          top_env := Env.add s v !top_env;
          s, (t, v)
      end in
      Printf.printf "val %s : %s = %s" name (string_of_polytype t) (string_of_value v)
    with
    | Interpret.Runtime_type_error -> print_string "Runtime type error."
    | Interpret.Var_not_found (name, _) -> Printf.printf "Error : variable %s not found." name
    | Parser.Error -> print_string "Error : syntax error."
    | Failure s -> print_string @@ "Failure : " ^ s
    | Reconstruction.Type_recursion (tvar, t) -> 
      Printf.printf "Error : type variable %d occurs recursively in %s." tvar (string_of_type t)
    | Reconstruction.Type_mismatch (t1, t2) ->
      Printf.printf "Error : type %s and %s mismatch." (string_of_type t1) (string_of_type t2)
  done

let () =
  loop ()