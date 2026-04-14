module type U = sig
  type 'a t
  val create : int -> 'a t
  val apply : 'a t -> ('a -> 'a) -> 'a -> int -> 'a
end

module Make (Uni : U) (Seq : SeqObject.S) = struct
  type 'a t = {
    u : ('a Seq.state * 'a option) Uni.t;
    num_threads : int;
  }

  let create num_threads = {
    u = Uni.create num_threads;
    num_threads;
  }

  let apply obj op tid =
    let invoc (state, _) = Seq.apply state op in
    let initial_obj = (Seq.empty, None) in
    let (_new_state, result) = Uni.apply obj.u invoc initial_obj tid in
    result
end