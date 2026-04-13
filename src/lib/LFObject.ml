module Make (Seq : SeqObject.S) = struct
  
  type 'a t = {
    lfu : ('a Seq.state * 'a option) LFUniversal.t;
    num_threads : int;
  }

  let create num_threads = {
    lfu = LFUniversal.create num_threads;
    num_threads;
  }

  let apply obj op tid =
    let invoc (state, _) = Seq.apply state op in
    let initial_obj = (Seq.empty, None) in
    let (_new_state, result) = LFUniversal.apply obj.lfu invoc initial_obj tid in
    result
end