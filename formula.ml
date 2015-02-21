type 'a t =
  | False
  | True
  | Atom of 'a
  | Neg of 'a t
  | And of 'a t * 'a t
  | Or of 'a t * 'a t
  | Imp of 'a t * 'a t
  | Iff of 'a t * 'a t

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

let to_string e =
  let rec to_string_pr pr =
    let prec i s = if i < pr then "("^s^")" else s in
    function
    | False -> "false"
    | True -> "true"
    | Atom _ -> invalid_arg "to_string"
    | Neg p -> let s = "~ " ^ (to_string_pr 10 p) in prec 10 s
    | And (p,q) -> 
       let s = (to_string_pr 9 p)^" /\\ "^(to_string_pr 8 q) in
       prec 8 s
    | Or (p,q) -> 
       let s = (to_string_pr 7 p)^" \\/ "^(to_string_pr 6 q) in
       prec 6 s
    | Imp (p,q) -> 
       let s = (to_string_pr 6 p)^" ==> "^(to_string_pr 4 q) in
       prec 4 s
    | Iff (p,q) -> 
       let s = (to_string_pr 5 p)^" <=> "^(to_string_pr 2 q) in
       prec 2 s
  in to_string_pr 0 e

let rec eval vl = function
  | False -> false
  | True -> true
  | Atom x -> vl x
  | Neg p -> not (eval vl p)
  | And (p,q) -> (eval vl p) && (eval vl q)
  | Or (p,q)  -> (eval vl p) || (eval vl q)
  | Imp (p,q) -> not (eval vl p) || (eval vl q)
  | Iff (p,q) -> (eval vl p) = (eval vl q)
