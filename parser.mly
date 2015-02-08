%{
  open Formula
%}

%token EOF
%token <int> INT
%token TRUE FALSE
%token NEG CONJ DISJ IMP IFF
%token LPAREN RPAREN

/* Highest to lowest precedece */
%left IFF
%left IMP
%left DISJ
%left CONJ
%nonassoc NEG

%start main
%type <char Formula.t> main

%%

main : expr EOF { $1 }

expr:
  | FALSE                   { False }
  | TRUE                    { True }
  | NEG expr                { Neg $2 }
  | expr CONJ expr          { And ($1,$3) }
  | expr DISJ expr          { Or ($1,$3) }
  | expr IMP expr           { Imp ($1,$3) }
  | expr IFF expr           { Iff ($1,$3) }
  | LPAREN expr RPAREN      { $2 }
;
