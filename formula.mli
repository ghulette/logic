type 'a t
val to_string : 'a t -> string
val of_string : string -> char t
val of_channel : in_channel -> char t

val mk_atom : 'a -> 'a t
val mk_neg : 'a t -> 'a t
val mk_and : 'a t -> 'a t -> 'a t
val mk_or : 'a t -> 'a t -> 'a t
val mk_imp : 'a t -> 'a t -> 'a t
val mk_iff : 'a t -> 'a t -> 'a t

val dest_atom : 'a t -> 'a                                
val dest_neg : 'a t -> 'a t
val dest_and : 'a t -> 'a t * 'a t
val dest_or : 'a t -> 'a t * 'a t
val dest_imp : 'a t -> 'a t * 'a t
val dest_iff : 'a t -> 'a t * 'a t

val antecedent : 'a t -> 'a t
val consequent : 'a t -> 'a t
val conjuncts : 'a t -> 'a t list
val disjuncts : 'a t -> 'a t list

val on_atoms : ('a -> 'b t) -> 'a t -> 'b t
val over_atoms : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val atom_union : ('a -> 'b list) -> 'a t -> 'b list
val eval : ('a -> bool) -> 'a t -> bool
val atoms : 'a t -> 'a list
val all_valuations : 'a list -> 'b list -> ('a * 'b) list list
val on_all_valuations :
  (('a -> bool) -> bool) -> ('a -> bool) -> 'a list -> bool
