module SeqStackAdapter = struct
  type 'a state = 'a Sequential.SequentialStack.state
  type 'a op = 'a Sequential.SequentialStack.op

  let empty = Sequential.SequentialStack.empty
  let apply state op = Sequential.SequentialStack.apply op state
end

module SeqQueueAdapter = struct
  type 'a state = 'a Sequential.SequentialQueue.state
  type 'a op = 'a Sequential.SequentialQueue.op

  let empty = Sequential.SequentialQueue.empty
  let apply state op = Sequential.SequentialQueue.apply op state
end

module SeqBstAdapter = struct
  type 'a state = 'a Sequential.SequentialBst.state
  type 'a op = 'a Sequential.SequentialBst.op

  let empty = Sequential.SequentialBst.empty
  let apply state op = Sequential.SequentialBst.apply state op
end

module SeqSortedListAdapter = struct
  type 'a state = 'a Sequential.SequentialSortedList.state
  type 'a op = 'a Sequential.SequentialSortedList.op

  let empty = Sequential.SequentialSortedList.empty
  let apply state op = Sequential.SequentialSortedList.apply state op
end

module InstrumentedStack = struct
  type 'a t = ('a SeqStackAdapter.state, 'a option) Src.WFUniversalStats.t

  let create = Src.WFUniversalStats.create

  let apply_with_stats obj op =
    let invoc state = SeqStackAdapter.apply state op in
    let initial_obj = SeqStackAdapter.empty in
    let (_next_obj, _result, stats) = Src.WFUniversalStats.apply_with_stats obj initial_obj invoc in
    (stats.Src.WFUniversalStats.helped, stats.Src.WFUniversalStats.own)
end

module InstrumentedQueue = struct
  type 'a t = ('a SeqQueueAdapter.state, 'a option) Src.WFUniversalStats.t

  let create = Src.WFUniversalStats.create

  let apply_with_stats obj op =
    let invoc state = SeqQueueAdapter.apply state op in
    let initial_obj = SeqQueueAdapter.empty in
    let (_next_obj, _result, stats) = Src.WFUniversalStats.apply_with_stats obj initial_obj invoc in
    (stats.Src.WFUniversalStats.helped, stats.Src.WFUniversalStats.own)
end

module InstrumentedBst = struct
  type 'a t = ('a SeqBstAdapter.state, 'a option) Src.WFUniversalStats.t

  let create = Src.WFUniversalStats.create

  let apply_with_stats obj op =
    let invoc state = SeqBstAdapter.apply state op in
    let initial_obj = SeqBstAdapter.empty in
    let (_next_obj, _result, stats) = Src.WFUniversalStats.apply_with_stats obj initial_obj invoc in
    (stats.Src.WFUniversalStats.helped, stats.Src.WFUniversalStats.own)
end

module InstrumentedSortedList = struct
  type 'a t = ('a SeqSortedListAdapter.state, 'a option) Src.WFUniversalStats.t

  let create = Src.WFUniversalStats.create

  let apply_with_stats obj op =
    let invoc state = SeqSortedListAdapter.apply state op in
    let initial_obj = SeqSortedListAdapter.empty in
    let (_next_obj, _result, stats) = Src.WFUniversalStats.apply_with_stats obj initial_obj invoc in
    (stats.Src.WFUniversalStats.helped, stats.Src.WFUniversalStats.own)
end

let chunks total parts =
  let base = total / parts in
  let rem = total mod parts in
  Array.init parts (fun i -> if i < rem then base + 1 else base)

let run_parallel num_threads f =
  let workers =
    Array.init (max 0 (num_threads - 1)) (fun i -> Domain.spawn (fun () -> f (i + 1)))
  in
  f 0;
  Array.iter Domain.join workers

let timed_seconds f =
  let t0 = Unix.gettimeofday () in
  f ();
  Unix.gettimeofday () -. t0

type stats = {
  helped : int;
  own : int;
  seconds : float;
}

let benchmark_stack ~num_ops =
  if num_ops < 0 then invalid_arg "benchmark_stack: num_ops must be >= 0";
  if num_ops mod 2 <> 0 then invalid_arg "benchmark_stack: num_ops must be even";
  let num_threads = 8 in
  let total_pairs = num_ops / 2 in
  let per_thread = chunks total_pairs num_threads in
  let stack = InstrumentedStack.create num_threads in
  let helped_by_thread = Array.make num_threads 0 in
  let own_by_thread = Array.make num_threads 0 in
  let seconds =
    timed_seconds (fun () ->
        run_parallel num_threads (fun tid ->
            let pairs = per_thread.(tid) in
            let local_helped = ref 0 in
            let local_own = ref 0 in
            for i = 0 to pairs - 1 do
              let (helped, own) =
                InstrumentedStack.apply_with_stats
                  stack
                  (Sequential.SequentialStack.Push (tid lsl 20 + i))
              in
              local_helped := !local_helped + helped;
              local_own := !local_own + own;
              let (helped, own) = InstrumentedStack.apply_with_stats stack Sequential.SequentialStack.Pop in
              local_helped := !local_helped + helped;
              local_own := !local_own + own
            done;
            helped_by_thread.(tid) <- !local_helped;
            own_by_thread.(tid) <- !local_own))
  in
  {
    helped = Array.fold_left ( + ) 0 helped_by_thread;
    own = Array.fold_left ( + ) 0 own_by_thread;
    seconds;
  }

let benchmark_queue ~num_ops =
  if num_ops < 0 then invalid_arg "benchmark_queue: num_ops must be >= 0";
  if num_ops mod 2 <> 0 then invalid_arg "benchmark_queue: num_ops must be even";
  let num_threads = 8 in
  let total_pairs = num_ops / 2 in
  let per_thread = chunks total_pairs num_threads in
  let queue = InstrumentedQueue.create num_threads in
  let helped_by_thread = Array.make num_threads 0 in
  let own_by_thread = Array.make num_threads 0 in
  let seconds =
    timed_seconds (fun () ->
        run_parallel num_threads (fun tid ->
            let pairs = per_thread.(tid) in
            let local_helped = ref 0 in
            let local_own = ref 0 in
            for i = 0 to pairs - 1 do
              let (helped, own) =
                InstrumentedQueue.apply_with_stats
                  queue
                  (Sequential.SequentialQueue.Enqueue (tid lsl 20 + i))
              in
              local_helped := !local_helped + helped;
              local_own := !local_own + own;
              let (helped, own) = InstrumentedQueue.apply_with_stats queue Sequential.SequentialQueue.Dequeue in
              local_helped := !local_helped + helped;
              local_own := !local_own + own
            done;
            helped_by_thread.(tid) <- !local_helped;
            own_by_thread.(tid) <- !local_own))
  in
  {
    helped = Array.fold_left ( + ) 0 helped_by_thread;
    own = Array.fold_left ( + ) 0 own_by_thread;
    seconds;
  }

let benchmark_bst ~num_ops =
  if num_ops < 0 then invalid_arg "benchmark_bst: num_ops must be >= 0";
  if num_ops mod 2 <> 0 then invalid_arg "benchmark_bst: num_ops must be even";
  let num_threads = 8 in
  let total_pairs = num_ops / 2 in
  let per_thread = chunks total_pairs num_threads in
  let bst = InstrumentedBst.create num_threads in
  let helped_by_thread = Array.make num_threads 0 in
  let own_by_thread = Array.make num_threads 0 in
  let seconds =
    timed_seconds (fun () ->
        run_parallel num_threads (fun tid ->
            let pairs = per_thread.(tid) in
            let local_helped = ref 0 in
            let local_own = ref 0 in
            for i = 0 to pairs - 1 do
              let (helped, own) =
                InstrumentedBst.apply_with_stats
                  bst
                  (Sequential.SequentialBst.Insert (tid lsl 20 + i))
              in
              local_helped := !local_helped + helped;
              local_own := !local_own + own;
              let (helped, own) = InstrumentedBst.apply_with_stats bst (Sequential.SequentialBst.Remove 0) in
              local_helped := !local_helped + helped;
              local_own := !local_own + own
            done;
            helped_by_thread.(tid) <- !local_helped;
            own_by_thread.(tid) <- !local_own))
  in
  {
    helped = Array.fold_left ( + ) 0 helped_by_thread;
    own = Array.fold_left ( + ) 0 own_by_thread;
    seconds;
  }

let benchmark_sortedlist ~num_ops =
  if num_ops < 0 then invalid_arg "benchmark_sortedlist: num_ops must be >= 0";
  if num_ops mod 2 <> 0 then invalid_arg "benchmark_sortedlist: num_ops must be even";
  let num_threads = 8 in
  let total_pairs = num_ops / 2 in
  let per_thread = chunks total_pairs num_threads in
  let sortedlist = InstrumentedSortedList.create num_threads in
  let helped_by_thread = Array.make num_threads 0 in
  let own_by_thread = Array.make num_threads 0 in
  let seconds =
    timed_seconds (fun () ->
        run_parallel num_threads (fun tid ->
            let pairs = per_thread.(tid) in
            let local_helped = ref 0 in
            let local_own = ref 0 in
            for i = 0 to pairs - 1 do
              let (helped, own) =
                InstrumentedSortedList.apply_with_stats
                  sortedlist
                  (Sequential.SequentialSortedList.Insert (tid lsl 20 + i))
              in
              local_helped := !local_helped + helped;
              local_own := !local_own + own;
              let (helped, own) = InstrumentedSortedList.apply_with_stats sortedlist (Sequential.SequentialSortedList.Remove 0) in
              local_helped := !local_helped + helped;
              local_own := !local_own + own
            done;
            helped_by_thread.(tid) <- !local_helped;
            own_by_thread.(tid) <- !local_own))
  in
  {
    helped = Array.fold_left ( + ) 0 helped_by_thread;
    own = Array.fold_left ( + ) 0 own_by_thread;
    seconds;
  }

let print_stats label ~num_ops stats =
  let total = stats.helped + stats.own in
  let help_ratio =
    if total = 0 then 0.0 else float_of_int stats.helped /. float_of_int total
  in
  Printf.printf "%s,threads=8,num_ops=%d,helped=%d,own=%d,help_ratio=%.6f,seconds=%.6f\n%!"
    label num_ops stats.helped stats.own help_ratio stats.seconds

let () =
  if Array.length Sys.argv <> 2 then
    invalid_arg "Usage: cost_of_helping <num_ops>";
  let num_ops = int_of_string Sys.argv.(1) in
  let stack_stats = benchmark_stack ~num_ops in
  let queue_stats = benchmark_queue ~num_ops in
  let bst_stats = benchmark_bst ~num_ops in
  let sortedlist_stats = benchmark_sortedlist ~num_ops in
  print_stats "stack" ~num_ops stack_stats;
  print_stats "queue" ~num_ops queue_stats;
  print_stats "bst" ~num_ops bst_stats;
  print_stats "sortedlist" ~num_ops sortedlist_stats