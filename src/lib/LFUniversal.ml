

type ('a,'b) t = {
  head : ('a,'b) Node.t array;
  tail : ('a,'b) Node.t;
  num_threads : int;
}

let create num_threads = 
  let tail = Node.create (fun x -> x) num_threads in 
  Node.set_seq tail 1;
  {
    head = Array.make num_threads tail;
    tail;
    num_threads
  }

(*new_obj is a newly constrcuted object*)
(*invoc is a partially applied function. Its only missing argument is the object. It returns a new object after applying the function *)
let apply lfu_obj invoc new_obj tid =
  let prefer = Node.create invoc lfu_obj.num_threads in
  let rec aux () =
    if Node.get_seq prefer <> 0 then ()
    else begin
    let before = Node.max lfu_obj.head in
    let after = CASConsensus.decide (Node.get_decide_next before) prefer tid in
    Node.set_next before after;
    Node.set_seq after (Node.get_seq before + 1);
    lfu_obj.head.(tid) <- after;
    aux ()
    end
  in
  aux ();
  let rec app current acc = 
    if current = Some prefer then (Node.get_invoc prefer) acc
    else begin match current with
    | Some node -> app (Node.get_next node) ((Node.get_invoc node) acc)
    | None -> failwith "This should never happen"
    end
  in
  app (Node.get_next lfu_obj.tail) new_obj
