
(** 'a is the type of the object on which the invoc function operates. 
'a is also the type of the object invoc function returns *)
type ('a,'b) t

(** Creates a new node with the given value and sequence number *)
val create : ('a -> 'b -> 'a * 'b) option -> int -> ('a,'b) t

(** Returns the node with the maximum sequence number in the array *)
val max : ('a,'b) t array -> ('a,'b) t

(** Updates the sequence number of the given node *)
val set_seq : ('a,'b) t -> int -> unit

(** Returns the sequence number of the given node *)
val get_seq : ('a,'b) t -> int

(** Returns the CAS consensus instance associated with the given node *)
val get_decide_next : ('a,'b) t -> ('a,'b) t CASConsensus.t

(** sets the next node for the given node *)
val set_next : ('a,'b) t -> ('a,'b) t -> unit

(** returns the next node for the given node *)
val get_next : ('a,'b) t -> ('a,'b) t option

(** returns the function stored in the given node *)
val get_invoc : ('a,'b) t -> ('a -> 'b -> 'a * 'b) option