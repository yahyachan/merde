{
  open Lexing
  open Parser
}

let var = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
    [' ' '\t' '\r'] { token lexbuf }
  | '\n'            { new_line lexbuf; token lexbuf }
  | ['0'-'9']+      { INT (int_of_string (lexeme lexbuf)) }
  | "true"          { TRUE }
  | "false"         { FALSE }
  | "fun"           { FUN }
  | "fix"           { FIX }
  | "=>"            { DARROW }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "let"           { LET }
  | "in"            { IN }
  | ";;"            { SEMISEMI }
  | '='             { EQUAL }
  | '<'             { LESS }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '+'             { PLUS }
  | '-'             { SUB }
  | '*'             { TIMES }
  | '/'             { DIV }
  | var             { VARIABLE (Lexing.lexeme lexbuf) }
  | eof             { EOF }


{}