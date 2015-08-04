module V = Valuation

type prop = string

type 'a t =
  | False
  | True
  | Atom of 'a
  | Not of 'a t
  | And of 'a t * 'a t
  | Or of 'a t * 'a t
  | Impl of 'a t * 'a t
  | Equiv of 'a t * 'a t

let to_string e =
  let rec to_string_pr pr =
    let prec i s = if i < pr then "(" ^ s ^ ")" else s in
    function
    | False -> "false"
    | True -> "true"
    | Atom x -> x
    | Not p -> let s = "~ " ^ (to_string_pr 10 p) in prec 10 s
    | And (p,q) ->
       let s = (to_string_pr 9 p)^{| /\ |}^(to_string_pr 8 q) in
       prec 8 s
    | Or (p,q) ->
       let s = (to_string_pr 7 p)^{| \/ |}^(to_string_pr 6 q) in
       prec 6 s
    | Impl (p,q) ->
       let s = (to_string_pr 6 p)^{| ==> |}^(to_string_pr 4 q) in
       prec 4 s
    | Equiv (p,q) ->
       let s = (to_string_pr 5 p)^{| <=> |}^(to_string_pr 2 q) in
       prec 2 s
  in to_string_pr 0 e

let print_formula ppf fm =
  Format.fprintf ppf "%s\n" (to_string fm);;

let print = print_formula Format.std_formatter

let rec of_ast = function
  | Ast.Const true -> True
  | Ast.Const false -> False
  | Ast.Var x -> Atom x
  | Ast.Not e -> Not (of_ast e)
  | Ast.Conn (Ast.And, e1, e2) -> And (of_ast e1, of_ast e2)
  | Ast.Conn (Ast.Or, e1, e2) -> Or (of_ast e1, of_ast e2)
  | Ast.Conn (Ast.Impl, e1, e2) -> Impl (of_ast e1, of_ast e2)
  | Ast.Conn (Ast.Equiv, e1, e2) -> Equiv (of_ast e1, of_ast e2)
  | _ -> failwith "Not a propositional formula"

let of_string s =
  Ast.of_string s |> of_ast

let of_channel ch =
  Ast.of_channel ch |> of_ast

let mk_atom x = Atom x
let mk_not p = Not p
let mk_and p q = And (p,q)
let mk_or p q = Or (p,q)
let mk_impl p q = Impl (p,q)
let mk_equiv p q = Equiv (p,q)
let dest_atom = function Atom x -> x | _ -> failwith "dest_atom"
let dest_not = function Not p -> p | _ -> failwith "dest_not"
let dest_and = function And (p,q) -> (p,q) | _ -> failwith "dest_and"
let dest_or = function Or (p,q) -> (p,q) | _ -> failwith "dest_or"
let dest_impl = function Impl (p,q) -> (p,q) | _ -> failwith "dest_impl"
let dest_equiv = function Equiv (p,q) -> (p,q) | _ -> failwith "dest_equiv"

let antecedent fm = fst (dest_impl fm)
let consequent fm = snd (dest_impl fm)

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
  | Not p -> Not (on_atoms f p)
  | And (p,q) -> And (on_atoms f p, on_atoms f q)
  | Or (p,q) -> Or (on_atoms f p, on_atoms f q)
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
  | Or (p,q) -> (eval p env) || (eval q env)
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
  let hdr = ats @ ["fm"] in
  Util.print_table 5 string_of_bool hdr rows

let tautology fm =
  V.on_all (atoms fm) (eval fm)

let unsatisfiable fm =
  tautology (Not fm)

let satisfiable fm =
  not (unsatisfiable fm)

let subst pf =
  on_atoms (fun p -> Partial.applyd pf p ~default:(Atom p))

let positive_lit = function Atom _ -> true | _ -> false
let negative_lit = function Not (Atom _) -> true | _ -> false
let negate = function Not p -> p | p -> Not p

(* Bottom-up traversal *)
let rec traverse f = function
  | Not p -> f (Not (traverse f p))
  | And (p,q) -> f (And (traverse f p, traverse f q))
  | Or (p,q) -> f (Or (traverse f p, traverse f q))
  | Impl (p,q) -> f (Impl (traverse f p, traverse f q))
  | Equiv (p,q) -> f (Equiv (traverse f p, traverse f q))
  | fm -> f fm

let dual fm =
  let xf = function
    | False -> True
    | True -> False
    | And (p,q) -> Or (p,q)
    | Or (p,q) -> And (p,q)
    | Impl (_,_) -> failwith "Cannot dualize ==>"
    | Equiv (_,_) -> failwith "Cannot dualize <=>"
    | fm -> fm
  in
  traverse xf fm

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
    | fm -> fm
  in
  traverse simpl fm

let exp_impl fm =
  let xf = function
    | Impl (p, q) -> Or (Not p, q)
    | fm -> fm
  in
  traverse xf fm

let exp_equiv fm =
  let xf = function
    | Equiv (p, q) -> And (Or (Not p, q), Or (Not q, p))
    | fm -> fm
  in
  traverse xf fm

let exp_nnf fm =
  let rec xf = function
    | Not (Not p) -> traverse xf p
    | Not (And (p, q)) -> traverse xf (Or (Not p, Not q))
    | Not (Or (p, q)) -> traverse xf (And (Not p, Not q))
    | fm -> fm
  in
  traverse xf fm

let nnf fm =
  fm |> psimplify |> exp_equiv |> exp_impl |> exp_nnf

let nenf fm =
  fm |> psimplify |> exp_impl |> exp_nnf

let list_conj = function
  | [] -> True
  | ps -> Util.fold_left_end mk_and ps

let list_disj = function
  | [] -> False
  | ps -> Util.fold_left_end mk_or ps
