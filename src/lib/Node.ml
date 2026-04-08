
type ('a,'b) t =
  {
    decide_next : 'a CASConsensus.t;
    mutable next : ('a,'b) t option;
    mutable seq : int;
    invoc : 'b -> 'b (*invoc is a function*)
  }

let create invoc num_threads = {
    decide_next = CASConsensus.create num_threads; 
    next = None; 
    seq = 0;
    invoc;
  }

let max arr = 
  let rec aux acc i = 
    if i >= Array.length arr then acc
    else if arr.(i).seq > acc.seq then aux arr.(i) (i+1)
    else aux acc (i+1)
  in
  aux arr.(0) 1

let set_seq r v =
  r.seq <- v

let get_seq r = r.seq

let get_next r = r.next

let set_next r next_node =
  r.next <- Some next_node

let get_decide_next r = r.decide_next

let get_invoc r = r.invoc