module V = Valuation

type 'a t = 'a Ast.t

let to_string = Ast.to_string

let print_formula ppf fm =
  Format.fprintf ppf "%s" (to_string Char.escaped fm);;

let of_string s =
  let lexbuf = Lexing.from_string s in
  Parser.main Lexer.token lexbuf

let of_channel ch =
  let lexbuf = Lexing.from_channel ch in
  Parser.main Lexer.token lexbuf

open Ast

let mk_atom x = Atom x
let mk_neg p = Not p
let mk_and p q = And (p,q)
let mk_or p q = Or (p,q)
let mk_imp p q = Impl (p,q)
let mk_iff p q = Equiv (p,q)
let dest_atom = function Atom x -> x | _ -> failwith "dest_atom"
let dest_neg = function Not p -> p | _ -> failwith "dest_neg"
let dest_and = function And (p,q) -> (p,q) | _ -> failwith "dest_and"
let dest_or = function Or (p,q) -> (p,q) | _ -> failwith "dest_or"
let dest_imp = function Impl (p,q) -> (p,q) | _ -> failwith "dest_imp"
let dest_iff = function Equiv (p,q) -> (p,q) | _ -> failwith "dest_iff"

let antecedent fm = fst (dest_imp fm)
let consequent fm = snd (dest_imp fm)

let rec conjuncts = function
  | And(p,q) -> conjuncts p @ conjuncts q
  | fm -> [fm]

let rec disjuncts = function
  | Or(p,q) -> disjuncts p @ disjuncts q
  | fm -> [fm]

(* Bottom-up traversal *)
let rec traverse f = function
  | Not p -> f (Not (traverse f p))
  | And (p,q) -> f (And (traverse f p, traverse f q))
  | Or  (p,q) -> f (Or (traverse f p, traverse f q))
  | Impl (p,q) -> f (Impl (traverse f p, traverse f q))
  | Equiv (p,q) -> f (Equiv (traverse f p, traverse f q))
  | _ as fm -> fm

let rec on_atoms f = function
  | False -> False
  | True -> True
  | Atom x -> f x
  | Not p -> Not (on_atoms f p)
  | And (p,q) -> And (on_atoms f p, on_atoms f q)
  | Or  (p,q) -> Or  (on_atoms f p, on_atoms f q)
  | Impl (p,q) -> Impl (on_atoms f p, on_atoms f q)
  | Equiv (p,q) -> Equiv (on_atoms f p, on_atoms f q)

let rec over_atoms f fm b =
  match fm with
  | Atom a -> f a b
  | Not p -> over_atoms f p b
  | And (p,q) | Or (p,q) | Impl (p,q) | Equiv (p,q) ->
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
  | Not p -> not (eval p env)
  | And (p,q) -> (eval p env) && (eval q env)
  | Or  (p,q) -> (eval p env) || (eval q env)
  | Impl (p,q) -> not (eval p env) || (eval q env)
  | Equiv (p,q) -> (eval p env) = (eval q env)

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
  tautology (Not fm)

let satisfiable fm =
  not (unsatisfiable fm)

let subst pf =
  on_atoms (fun p -> Partial.applyd pf p ~default:(Atom p))

let rec dual = function
  | False -> True
  | True -> False
  | Atom x as a -> a
  | Not p -> Not (dual p)
  | And (p,q) -> Or (dual p, dual q)
  | Or (p,q) -> And (dual p, dual q)
  | _ -> failwith "Cannot dualize formulas with ==> or <=>"
                  
let psimplify fm =
  let simpl = function
    | Not False -> True
    | Not True -> False
    | Not (Not p) -> p
    | And (p, False) | And (False, p) -> False
    | And (p, True)  | And (True, p) -> p
    | Or (p, False) | Or (False, p) -> p
    | Or (p, True)  | Or (True, p) -> True
    | Impl (False, p) | Impl (p, True) -> True
    | Impl (True, p) -> p
    | Impl (p, False) -> Not p
    | Equiv (p, True)  | Equiv (True, p) -> p
    | Equiv (p, False) | Equiv (False, p) -> Not p
    | _ as fm -> fm
  in traverse simpl fm

let positive_lit = function Atom _ -> true | _ -> false
let negative_lit = function Not (Atom _) -> true | _ -> false
let negate = function Not p -> p | p -> Not p

let nnf fm =
  let rec nnf_aux = function
    | And (p, q) -> And (nnf_aux p, nnf_aux q)
    | Or (p, q) -> Or (nnf_aux p, nnf_aux q)
    | Impl (p, q) -> Or (nnf_aux (Not p), nnf_aux q)
    | Equiv (p, q) -> Or (And (nnf_aux p, nnf_aux q),
                          And (nnf_aux (Not p), nnf_aux (Not q)))
    | Not (Not p) -> nnf_aux p
    | Not (And (p, q)) -> Or (nnf_aux (Not p), nnf_aux (Not q))
    | Not (Or (p, q)) -> And (nnf_aux (Not p), nnf_aux (Not q))
    | Not (Impl (p, q)) -> And (nnf_aux p, nnf_aux (Not q))
    | Not (Equiv (p, q)) -> Or (And (nnf_aux p, nnf_aux (Not q)),
                                And (nnf_aux (Not p), nnf_aux q))
    | _ as fm -> fm
  in
  nnf_aux (psimplify fm)
