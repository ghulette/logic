%{
  open Ast
%}

%token EOF
%token <int> INT
%token <char> CHAR
%token TRUE FALSE
%token NEG CONJ DISJ IMP IFF
%token LPAREN RPAREN

(* Highest to lowest precedece *)
%right IFF
%right IMP
%right DISJ
%right CONJ
%nonassoc NEG

%start main
%type <char Ast.t> main

%%

main : expr EOF { $1 }

expr:
  | FALSE                   { False }
  | TRUE                    { True }
  | CHAR                    { Atom $1 }
  | NEG expr                { Not $2 }
  | expr CONJ expr          { And ($1,$3) }
  | expr DISJ expr          { Or ($1,$3) }
  | expr IMP expr           { Impl ($1,$3) }
  | expr IFF expr           { Equiv ($1,$3) }
  | LPAREN expr RPAREN      { $2 }
;
