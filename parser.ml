open MParser
open MParser_PCRE
open Tokens

exception Syntax_error

type id = string
type quant = Forall | Exists
type conn = And | Or | Impl | Equiv

type t =
  | Const of bool
  | Var of id
  | Not of t
  | Conn of conn * t * t
  | Quant of quant * id * t

let lexeme p = p >>= fun e -> spaces >> return e

let ident = lexeme (many1_chars lowercase)

let operators =
  let infix sym f assoc = Infix (symbol sym >> return f, assoc) in
  let connect c p q = Conn (c,p,q) in
  [
    [infix {|/\|} (connect And) Assoc_right];
    [infix {|\/|} (connect Or) Assoc_right];
    [infix {|==>|} (connect Impl) Assoc_right];
    [infix {|<=>|} (connect Equiv) Assoc_right];
  ]

let rec term s =
  choice [
      symbol "true" >> return (Const true);
      symbol "false" >> return (Const false);
      (symbol "~" >> term >>= fun e -> return (Not e));
      quant "forall" Forall;
      quant "exists" Exists;
      (ident >>= fun x -> return (Var x));
      parens expr
    ] s

and quant sym q =
  symbol sym >>
    ident >>= fun x ->
  comma >>
    expr >>= fun e ->
  return (Quant (q,x,e))

and expr s =
  s |> (expression operators term)

let formula s =
  s |> (spaces >> expr >>= fun e -> eof >> return e)


let of_string s =
  match parse_string formula s () with
  | Success x -> x
  | Failed (msg, _) ->
     print_string msg;
     raise Syntax_error

let of_channel ch =
  match parse_channel formula ch () with
  | Success x -> x
  | Failed (msg, _) ->
     print_string msg;
     raise Syntax_error
