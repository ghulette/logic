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

val of_string : string -> t
val of_channel : in_channel -> t
