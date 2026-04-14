module type U = sig
  type ('a, 'b) t
  val create : int -> ('a, 'b) t
  val apply : ('a,'b) t -> 'a -> ('a -> 'a * 'b) -> int -> 'a * 'b
end

module Make (Uni : U) (Seq : Sequential.SeqObject.S) = struct
  type 'a t = {
    u : ('a Seq.state , 'a option) Uni.t;
    num_threads : int;
  }

  let create num_threads = {
    u = Uni.create num_threads;
    num_threads;
  }

  let apply obj op tid =
    let invoc state = Seq.apply state op in
    let initial_obj = Seq.empty in
    let (_new_state, result) = Uni.apply obj.u initial_obj invoc tid in
    result
end