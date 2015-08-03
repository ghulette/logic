open MParser
open MParser_PCRE
open Tokens

exception Syntax_error

type t =
  | True
  | False
  | Atom of char
  | Not of t
  | And of t * t
  | Or of t * t

let lexeme p =
  spaces >> p

let atom =
  (lexeme (string "true") >> return True)
  <|> (lexeme (string "false") >> return False)
  <|> (lexeme letter >>= fun p -> return (Atom p))

let rec expr1 =
  (symbol "~" >> expr1)
  <|> expr2

and expr2 =
  (expr2 >>= fun e1 ->
  symbol {|/\|} >>= fun _ ->
  expr2 >>= fun e2 ->
  return (And (e1, e2)))



                    (*
let rec term s = (parens expr <|> decimal) s

and expr s = expression operators term s

let eval s =
  match parse_string expr s () with
  | Success x -> x
  | Failed (msg, _) ->
     print_string msg;
     raise Syntax_error
                     *)
