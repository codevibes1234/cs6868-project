module Make (Seq : SeqObject.S) : sig
  type 'a t

  (** [create num_threads] creates a new lock-free object for [num_threads] threads. *)
  val create : int -> 'a t

  (** [apply obj op tid] applies [op] to the lock-free object [obj] on behalf of
      thread [tid]. Returns the result of the operation. *)
  val apply : 'a t -> 'a Seq.op -> int -> 'a option
end