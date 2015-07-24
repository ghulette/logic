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


type 'a valuation = 'a -> bool

let empty = fun _ -> false
let extend x v env = fun y -> if x = y then v else env x
let lookup x env = env x

         
let rec eval fm env =
  match fm with
  | False -> false
  | True -> true
  | Atom x -> lookup x env
  | Neg p -> not (eval p env)
  | And (p,q) -> (eval p env) && (eval q env)
  | Or  (p,q) -> (eval p env) || (eval q env)
  | Imp (p,q) -> not (eval p env) || (eval q env)
  | Iff (p,q) -> (eval p env) = (eval q env)

let atoms fm =
  atom_union (fun a -> [a]) fm

let rec all_valuations ks vs =
  let (>>=) m f = List.flatten (List.map f m) in
  match ks with
  | [] -> [[]]
  | k::ks' ->
    List.map (fun v -> (k,v)) vs >>= fun kv ->
    all_valuations ks' vs >>= fun vl ->
    [kv :: vl]

let print_truth_table fm =
  let envs = all_valuations (atoms fm) [true; false] in
  let rs = List.map (eval fm) (List.map Util.partial envs) in
  let rows = List.map2 (fun vs r -> (List.map snd vs) @ [r]) envs rs in
  let hdr = (List.map Char.escaped (atoms fm)) @ ["fm"] in
  Util.print_table 5 string_of_bool hdr rows

                            
let on_all_valuations fm f = 
  let rec on_all_valuations_aux env = function
    | [] -> f env
    | x::xs ->
       on_all_valuations_aux (extend x false env) xs
       && on_all_valuations_aux (extend x true env) xs
  in
  on_all_valuations_aux (fun _ -> false) (atoms fm)
                   
let tautology fm =
  on_all_valuations fm (eval fm)

let unsatisfiable fm =
  tautology (Neg fm)
                    
let satisfiable fm =
  not (unsatisfiable fm)
