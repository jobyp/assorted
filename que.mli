type 'a t
exception Queue_empty
val create : unit -> 'a t
val enque : 'a t -> 'a -> unit
val deque : 'a t -> 'a
