module Make (Seq : SeqObject.S) = struct
  
  type 'a t = {
    wfu : ('a Seq.state * 'a option) WFUniversal.t;
    num_threads : int;
  }

  let create num_threads = {
    wfu = WFUniversal.create num_threads;
    num_threads;
  }

  let apply obj op tid =
    let invoc (state, _) = Seq.apply state op in
    let initial_obj = (Seq.empty, None) in
    let (_new_state, result) = WFUniversal.apply obj.wfu invoc initial_obj tid in
    result
end