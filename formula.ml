type 'a t =
  | False
  | True
  | Atom of 'a
  | Neg of 'a t
  | And of 'a t * 'a t
  | Or of 'a t * 'a t
  | Imp of 'a t * 'a t
  | Iff of 'a t * 'a t

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

let mk_neg p = Neg p
let mk_and p q = And (p,q)
let mk_or p q = Or (p,q)
let mk_imp p q = Imp (p,q)
let mk_iff p q = Iff (p,q)
let dest_neg = function Neg p -> p | _ -> failwith "dest_neg"
let dest_and = function And (p,q) -> (p,q) | _ -> failwith "dest_and"
let dest_or = function Or (p,q) -> (p,q) | _ -> failwith "dest_or"
let dest_imp = function Imp (p,q) -> (p,q) | _ -> failwith "dest_imp"
let dest_iff = function Iff (p,q) -> (p,q) | _ -> failwith "dest_iff"

let antecedent fm = fst (dest_imp fm)
let consequent fm = snd (dest_imp fm)

let rec conjuncts = function 
  | And(p,q) -> conjuncts p @ conjuncts q 
  | fm -> [fm]

let rec disjuncts = function 
  | Or(p,q) -> disjuncts p @ disjuncts q
  | fm -> [fm]

let rec on_atoms f = function
  | False -> False
  | True -> True
  | Atom x -> f x
  | Neg p -> Neg (on_atoms f p)
  | And (p,q) -> And (on_atoms f p, on_atoms f q)
  | Or (p,q) -> Or (on_atoms f p, on_atoms f q)
  | Imp (p,q) -> Imp (on_atoms f p, on_atoms f q)
  | Iff (p,q) -> Iff (on_atoms f p, on_atoms f q)

let rec over_atoms f fm b =
  match fm with
  | Atom a -> f a b
  | Neg p -> over_atoms f p b
  | And (p,q) | Or (p,q) | Imp (p,q) | Iff (p,q) ->
    over_atoms f p (over_atoms f q b)
  | _ -> b

let atom_union f fm =
  let setify xs = List.sort_uniq compare xs in
  setify (over_atoms (fun h t -> (f h) @ t) fm [])

let rec eval vl = function
  | False -> false
  | True -> true
  | Atom x -> vl x
  | Neg p -> not (eval vl p)
  | And (p,q) -> (eval vl p) && (eval vl q)
  | Or (p,q)  -> (eval vl p) || (eval vl q)
  | Imp (p,q) -> not (eval vl p) || (eval vl q)
  | Iff (p,q) -> (eval vl p) = (eval vl q)

let atoms fm = atom_union (fun a -> [a]) fm

let rec all_valuations ks vs =
  let (>>=) m f = List.flatten (List.map f m) in
  match ks with
  | [] -> [[]]
  | k::ks' ->
     List.map (fun v -> (k,v)) vs >>= fun kv ->
     all_valuations ks' vs >>= fun vl ->
     [kv :: vl]
