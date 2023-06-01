{
  open Lexing
  open Parser
}

let var = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let variant = '`' var

rule token = parse
    [' ' '\t' '\r'] { token lexbuf }
  | '\n'            { new_line lexbuf; token lexbuf }
  | ['0'-'9']+      { INT (int_of_string (lexeme lexbuf)) }
  | "true"          { TRUE }
  | "false"         { FALSE }
  | "fun"           { FUN }
  | "fix"           { FIX }
  | "match"         { MATCH }
  | "=>"            { DARROW }
  | "->"            { SARROW }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "with"          { WITH }
  | "end"           { END }
  | "let"           { LET }
  | "in"            { IN }
  | ";;"            { SEMISEMI }
  | "#q"            { TOPEXIT }
  | '='             { EQUAL }
  | '<'             { LESS }
  | '('             { LPAREN }
  | ')'             { RPAREN }
  | '+'             { PLUS }
  | '-'             { SUB }
  | '*'             { TIMES }
  | '/'             { DIV }
  | '.'             { DOT }
  | '{'             { LBRACE }
  | '}'             { RBRACE }
  | ','             { COMMA }
  | '|'             { VBAR }
  | var             { VARIABLE (Lexing.lexeme lexbuf) }
  | variant         { VARIANT (Lexing.lexeme lexbuf) }
  | eof             { EOF }


{}