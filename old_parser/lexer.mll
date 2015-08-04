{
open Parser
}

rule token = parse
| [' ' '\t' '\n']           { token lexbuf }
| ['0'-'9']+ as lxm         { INT (int_of_string lxm) }
| ['a'-'z' 'A'-'Z'] as lxm  { CHAR lxm }
| '('                       { LPAREN }
| ')'                       { RPAREN }
| '~'                       { NEG }
| "/\\"                     { CONJ }
| "\\/"                     { DISJ }
| "==>"                     { IMP }
| "<=>"                     { IFF }
| "true"                    { TRUE }
| "false"                   { FALSE }
| eof                       { EOF }
