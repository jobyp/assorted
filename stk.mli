type 'a t
exception Stack_empty
val create : unit -> 'a t
val push : 'a t -> 'a -> unit
val pop : 'a t -> 'a
val is_empty : 'a t -> bool
val iter : ('a -> unit) -> 'a t -> unit
