type 'a t = 'a Ast.t

let to_string = Ast.to_string

let of_string s =
  let lexbuf = Lexing.from_string s in
  Parser.main Lexer.token lexbuf

let of_channel ch =
  let lexbuf = Lexing.from_channel ch in
  Parser.main Lexer.token lexbuf

open Ast

let mk_atom x = Atom x
let mk_neg p = Neg p
let mk_and p q = And (p,q)
let mk_or p q = Or (p,q)
let mk_imp p q = Imp (p,q)
let mk_iff p q = Iff (p,q)
let dest_atom = function Atom x -> x | _ -> failwith "dest_atom"
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
  | Or  (p,q) -> Or (on_atoms f p, on_atoms f q)
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
  | Or  (p,q) -> (eval vl p) || (eval vl q)
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

let rec on_all_valuations f v = function
  | [] -> f v
  | p::ps ->
    let v' t q = if q = p then t else v q in
    on_all_valuations f (v' false) ps && on_all_valuations f (v' true) ps

let print_truth_table f =
  let vls = all_valuations (atoms f) [true; false] in
  let rs = List.map (fun vl -> eval vl f) (List.map Util.partial vls) in
  let rows = List.map2 (fun vs r -> (List.map snd vs) @ [r]) vls rs in
  let hdr = (List.map Char.escaped (atoms f)) @ ["fm"] in
  Util.print_table 5 string_of_bool hdr rows
