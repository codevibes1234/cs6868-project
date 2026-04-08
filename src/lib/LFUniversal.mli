

type ('a,'b) t

(** [create num_threads] creates a new universal construction instance for num_threads threads. *)
val create : int -> ('a,'b) t

(** [apply lf_universal new_obj prefer] applies the operation represented by [new_obj] to the universal construction instance [lf_universal], with preference for the node [prefer]. *)
val apply : ('a,'b) t -> ('b -> 'b) -> int -> 'b