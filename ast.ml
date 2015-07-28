type 'a t =
  | False
  | True
  | Atom of 'a
  | Not of 'a t
  | And of 'a t * 'a t
  | Or of 'a t * 'a t
  | Impl of 'a t * 'a t
  | Equiv of 'a t * 'a t

let to_string atom_pr e =
  let rec to_string_pr pr =
    let prec i s = if i < pr then "(" ^ s ^ ")" else s in
    function
    | False -> "false"
    | True -> "true"
    | Atom x -> atom_pr x
    | Not p -> let s = "~ " ^ (to_string_pr 10 p) in prec 10 s
    | And (p,q) -> 
       let s = (to_string_pr 9 p)^" /\\ "^(to_string_pr 8 q) in
       prec 8 s
    | Or (p,q) -> 
       let s = (to_string_pr 7 p)^" \\/ "^(to_string_pr 6 q) in
       prec 6 s
    | Impl (p,q) -> 
       let s = (to_string_pr 6 p)^" ==> "^(to_string_pr 4 q) in
       prec 4 s
    | Equiv (p,q) -> 
       let s = (to_string_pr 5 p)^" <=> "^(to_string_pr 2 q) in
       prec 2 s
  in to_string_pr 0 e
