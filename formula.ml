module V = Valuation

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
  | Or  (p,q) -> Or  (on_atoms f p, on_atoms f q)
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

let rec eval fm env =
  match fm with
  | False -> false
  | True -> true
  | Atom x -> V.lookup x env
  | Neg p -> not (eval p env)
  | And (p,q) -> (eval p env) && (eval q env)
  | Or  (p,q) -> (eval p env) || (eval q env)
  | Imp (p,q) -> not (eval p env) || (eval q env)
  | Iff (p,q) -> (eval p env) = (eval q env)

let atoms fm =
  atom_union (fun a -> [a]) fm

let print_truth_table fm =
  let ats = atoms fm in
  let envs = V.all ats in
  let rs = List.map (eval fm) envs in
  let rowf env = List.map (fun x -> V.lookup x env) ats in
  let vals = List.map rowf envs in
  let rows = List.map2 (fun vl r -> vl @ [r]) vals rs in
  let hdr = (List.map Char.escaped ats) @ ["fm"] in
  Util.print_table 5 string_of_bool hdr rows

let tautology fm =
  V.on_all (atoms fm) (eval fm)

let unsatisfiable fm =
  tautology (Neg fm)

let satisfiable fm =
  not (unsatisfiable fm)

let subst pf =
  on_atoms (fun p -> Partial.applyd pf p ~default:(Atom p))

let rec dual = function
  | False -> True
  | True -> False
  | Atom x as a -> a
  | Neg p -> Neg (dual p)
  | And (p,q) -> Or (dual p, dual q)
  | Or (p,q) -> And (dual p, dual q)
  | _ -> failwith "Cannot dualize formulas with ==> or <=>"

                  
