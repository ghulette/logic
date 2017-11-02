type prop
type 'a t

val to_string : prop t -> string
val print_formula : Format.formatter -> prop t -> unit
val print : prop t -> unit
val of_string : string -> prop t
val of_channel : in_channel -> prop t

val mk_atom : 'a -> 'a t
val mk_not : 'a t -> 'a t
val mk_and : 'a t -> 'a t -> 'a t
val mk_or : 'a t -> 'a t -> 'a t
val mk_impl : 'a t -> 'a t -> 'a t
val mk_equiv : 'a t -> 'a t -> 'a t

val dest_atom : 'a t -> 'a
val dest_not : 'a t -> 'a t
val dest_and : 'a t -> 'a t * 'a t
val dest_or : 'a t -> 'a t * 'a t
val dest_impl : 'a t -> 'a t * 'a t
val dest_equiv : 'a t -> 'a t * 'a t

val antecedent : 'a t -> 'a t
val consequent : 'a t -> 'a t
val conjuncts : 'a t -> 'a t list
val disjuncts : 'a t -> 'a t list

val on_atoms : ('a -> 'b t) -> 'a t -> 'b t
val over_atoms : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val atom_union : ('a -> 'b list) -> 'a t -> 'b list
val atoms : 'a t -> 'a list

val eval : 'a t -> 'a Valuation.t -> bool

val print_truth_table : prop t -> unit

val tautology : 'a t -> bool
val unsatisfiable : 'a t -> bool
val satisfiable : 'a t -> bool

val subst : ('a, 'a t) Partial.t -> 'a t -> 'a t

val traverse : ('a t -> 'a t) -> 'a t -> 'a t

(* dual (dual p) = p *)
(* forall v, eval (dual p) v = not(eval p (not ** v)) *)
val dual : 'a t -> 'a t

val psimplify : 'a t -> 'a t
val negative_lit : 'a t -> bool
val positive_lit : 'a t -> bool
val negate : 'a t -> 'a t

val nnf : 'a t -> 'a t
val nenf : 'a t -> 'a t
val dnf : 'a t -> 'a t