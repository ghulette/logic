type 'a t

val empty : 'a t
val extend : 'a -> bool -> 'a t -> 'a t
val lookup : 'a -> 'a t -> bool
val lookupd : 'a -> 'a t -> bool option
val all : 'a list -> 'a t list
