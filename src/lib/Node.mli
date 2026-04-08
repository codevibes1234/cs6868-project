
type ('a,'b) t

(** Creates a new node with the given value and sequence number *)
val create : ('b -> 'b) -> int -> ('a,'b) t

(** Returns the node with the maximum sequence number in the array *)
val max : ('a,'b) t Array.t -> ('a,'b) t

(** Updates the sequence number of the given node *)
val set_seq : ('a,'b) t -> int -> unit

val get_seq : ('a,'b) t -> int

(** Returns the CAS consensus instance associated with the given node *)
val get_decide_next : ('a,'b) t -> 'a CASConsensus.t

val set_next : ('a,'b) t -> ('a,'b) t -> unit

val get_next : ('a,'b) t -> ('a,'b) t option

val get_invoc : ('a,'b) t -> ('b -> 'b)