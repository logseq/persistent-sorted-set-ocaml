type 'a t

val create : unit -> 'a t
val set : 'a t -> 'a -> unit
val get : 'a t -> 'a option
