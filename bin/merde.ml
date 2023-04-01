open Lib.Syntax

let top_env = ref (Env.empty)
let string_of_value = function
  | VInt x -> string_of_int x
  | VBool b -> string_of_bool b
  | VClosure _ -> "<fun>"

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
  let parser = Lib.Parser.toplevel Lib.Lexer.token in
  while true do
    print_string "\nmerde> ";
    flush stdout;
    let buf = Lexing.from_string (read_command ()) in
    let name, v = begin
      match parser buf with
      | TopTerm t -> "-", Lib.Interpret.eval !top_env t
      | TopDef (s, t) ->
        let th = Lib.Interpret.eval !top_env t in
        top_env := Env.add s th !top_env;
        s, th
    end in
    Printf.printf "val %s = %s" name (string_of_value v)
  done

let () =
  loop ()