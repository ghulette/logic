type ('a, 'b) t

val undefined : ('a, 'b) t
val is_undefined : ('a, 'b) t -> bool
val singleton : 'a -> 'b -> ('a, 'b) t
val defined : 'a -> ('a, 'b) t -> bool
val undefine : 'a -> ('a, 'b) t -> ('a, 'b) t
val update : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
val apply : ('a, 'b) t -> 'a -> 'b
val applyd : ('a, 'b) t -> 'a -> default:'b -> 'b
val map : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

module Infix :
sig
  val (|=>) : 'a -> 'b -> ('a, 'b) t
  val (|->) : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
end
