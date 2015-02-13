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
  | Neg p -> "~ " ^ (to_string p)
  | And (p,q) -> (to_string p) ^ " /\\ " ^ (to_string q)
  | Or (p,q) -> (to_string p) ^ " \\/ " ^ (to_string q)
  | Imp (p,q) -> (to_string p) ^ " ==> " ^ (to_string q)
  | Iff (p,q) -> (to_string p) ^ " <=> " ^ (to_string q)

let print e =
  print_endline (to_string e)

let intro_neg p = Neg p
let intro_and p q = And (p,q)
let intro_or p q = Or (p,q)
let intro_imp p q = Imp (p,q)
let intro_iff p q = Iff (p,q)
let elim_neg = function Neg p -> p | _ -> failwith "elim_neg"
let elim_and = function And (p,q) -> (p,q) | _ -> failwith "elim_and"
let elim_or = function Or (p,q) -> (p,q) | _ -> failwith "elim_or"
let elim_imp = function Imp (p,q) -> (p,q) | _ -> failwith "elim_imp"
let elim_iff = function Iff (p,q) -> (p,q) | _ -> failwith "elim_iff"
