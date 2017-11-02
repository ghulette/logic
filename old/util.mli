val compose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
val non : ('a -> bool) -> 'a -> bool
val replicate : 'a -> int -> 'a list
val seq : int -> int list
val time : ('a -> 'b) -> 'a -> 'b

val fold_left1 : ('a -> 'a -> 'a) -> 'a list -> 'a
val fold_right1 : ('a -> 'a -> 'a) -> 'a list -> 'a

val print_table :
  int -> ('a -> string) -> string list -> 'a list list -> unit

module Infix :
sig
  val ( ** ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
end
