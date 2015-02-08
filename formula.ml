type 'a t =
  | False
  | True
  | Atom of 'a
  | Neg of 'a t
  | And of 'a t * 'a t
  | Or of 'a t * 'a t
  | Imp of 'a t * 'a t
  | Iff of 'a t * 'a t

let rec to_string = function
  | False -> "false"
  | True -> "true"
  | Neg p -> "~" ^ (to_string p)
  | And (p,q) -> (to_string p) ^ "/\\" ^ (to_string q)
  | Or (p,q) -> (to_string p) ^ "\\/" ^ (to_string q)
  | Imp (p,q) -> (to_string p) ^ "==>" ^ (to_string q)
  | Iff (p,q) -> (to_string p) ^ "<=>" ^ (to_string q)
