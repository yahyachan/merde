%{
  open Syntax

  let fill_pos e pos = { e; pos }
%}

%token PLUS
%token SUB
%token TIMES
%token DIV
%token EQUAL
%token LESS
%token <int> INT
%token TRUE FALSE COMMA VBAR
%token <Syntax.varname> VARIABLE
%token IF THEN ELSE
%token FUN FIX DARROW
%token LPAREN RPAREN LBRACE RBRACE
%token SEMISEMI
%token LET IN
%token EOF DOT

%start <Syntax.command> toplevel
%start <Syntax.command list> file

%nonassoc IN
%nonassoc DARROW
%nonassoc ELSE
%nonassoc EQUAL LESS
%left PLUS SUB
%left TIMES DIV

%%

file:
  | EOF { [] }

toplevel:
  | t = term; SEMISEMI { TopTerm t }
  | LET; v = VARIABLE; EQUAL; t = term; SEMISEMI { TopDef (v, t) }

term: preprocess(plain_term) { $1 }
plain_term:
  | e = plain_app_term { e }
  | e1 = term; PLUS; e2 = term { Binop (Plus, e1, e2) }
  | e1 = term; SUB; e2 = term { Binop (Sub, e1, e2) }
  | e1 = term; TIMES; e2 = term { Binop (Times, e1, e2) }
  | e1 = term; DIV; e2 = term { Binop (Div, e1, e2) }
  | e1 = term; EQUAL; e2 = term { Binop (Equal, e1, e2) }
  | e1 = term; LESS; e2 = term { Binop (Less, e1, e2) }
  | IF; e1 = term; THEN; e2 = term; ELSE; e3 = term { If (e1, e2, e3) }
  | FUN; xs = arg_list; DARROW; e = term { 
    let pos = Some ($startpos, $endpos) in
    (List.fold_right (fun x y -> fill_pos (Fun (x, y)) pos) xs e).e
  }
  | FIX; self = VARIABLE; DARROW; body = term { Fix (self, body) }
  | LET; x = VARIABLE; EQUAL; a = term; IN; b = term { Let (x, a, b) }

app_term: preprocess(plain_app_term) { $1 }
plain_app_term:
  | e = plain_simple_term { e }
  | e1 = app_term; e2 = simple_term { Apply (e1, e2) }

simple_term: preprocess(plain_simple_term) { $1 }
plain_simple_term:
  | x = VARIABLE
    { Var x }
  | x = INT
    { Int x }
  | TRUE
    { Bool true }
  | FALSE
    { Bool false }
  | r = simple_term; DOT; field = VARIABLE
    { RecordSelect (r, field) }
  | LBRACE RBRACE { RecordEmpty }
  | LBRACE r = simple_term SUB field = VARIABLE RBRACE
    { RecordRestrict (r, field) }
  | LBRACE ls = set_comma_list RBRACE
    { RecordExtension (ls, For_test.fill RecordEmpty) }
  | LBRACE ls = set_comma_list VBAR rs = term RBRACE
    { RecordExtension (ls, rs) }
  | LPAREN; e = plain_term; RPAREN
    { e }

set_comma_list:
  | x = VARIABLE; EQUAL; v = term { Env.singleton x [v] }
  | x = VARIABLE; EQUAL; v = term; COMMA; xs = set_comma_list
    { insert_lenv xs x v }
arg_list:
  | x = VARIABLE { [x] }
  | x = VARIABLE; xs = arg_list { x :: xs }

preprocess(X):
  x = X
  { fill_pos x @@ Some ($startpos, $endpos) }