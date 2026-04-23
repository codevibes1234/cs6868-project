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

module LFUniversalStack = Src.Universal.Make (Src.LFUniversal) (SeqStackAdapter)
module WFUniversalStack = Src.Universal.Make (Src.WFUniversal) (SeqStackAdapter)
module LFUniversalQueue = Src.Universal.Make (Src.LFUniversal) (SeqQueueAdapter)
module WFUniversalQueue = Src.Universal.Make (Src.WFUniversal) (SeqQueueAdapter)

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

type bench_stats = {
  seconds : float;
  throughput : float;
}

let throughput total_ops seconds =
  if seconds <= 0.0 then infinity else float_of_int total_ops /. seconds

let benchmark_stack ~num_threads ~total_ops ~make_universal ~push ~pop =
  let per_thread = chunks total_ops num_threads in
  let obj = make_universal num_threads in
  let seconds =
    timed_seconds (fun () ->
        run_parallel num_threads (fun tid ->
            let pairs = per_thread.(tid) in
            for i = 0 to pairs - 1 do
              ignore (push obj (tid lsl 20 + i));
              ignore (pop obj)
            done))
  in
  {
    seconds;
    throughput = throughput (total_ops * 2) seconds;
  }

let benchmark_queue ~num_threads ~total_ops ~make_universal ~enq ~deq =
  let per_thread = chunks total_ops num_threads in
  let obj = make_universal num_threads in
  let seconds =
    timed_seconds (fun () ->
        run_parallel num_threads (fun tid ->
            let pairs = per_thread.(tid) in
            for i = 0 to pairs - 1 do
              ignore (enq obj (tid lsl 20 + i));
              ignore (deq obj)
            done))
  in
  {
    seconds;
    throughput = throughput (total_ops * 2) seconds;
  }

let append_csv_row oc ~object_name ~implementation ~num_ops ~seconds ~throughput ~penalty_pct =
  Printf.fprintf oc "%s,%s,%d,%.9f,%.3f,%.2f\n"
    object_name implementation num_ops seconds throughput penalty_pct;
  flush oc

let benchmark ~num_ops =
  if num_ops <= 0 then invalid_arg "benchmark: num_ops must be > 0";
  if num_ops mod 2 <> 0 then invalid_arg "benchmark: num_ops must be even";
  let num_threads = 8 in
  let output_file = "throughput.csv" in
  let oc = open_out_gen [ Open_creat; Open_text; Open_trunc; Open_wronly ] 0o644 output_file in
  Printf.fprintf oc "object,implementation,num_ops,seconds,ops_per_sec,penalty_pct\n";

  let stack_lf =
    benchmark_stack
      ~num_threads
      ~total_ops:(num_ops / 2)
      ~make_universal:LFUniversalStack.create
      ~push:(fun obj value -> LFUniversalStack.apply obj (Sequential.SequentialStack.Push value))
      ~pop:(fun obj -> LFUniversalStack.apply obj Sequential.SequentialStack.Pop)
  in
  append_csv_row oc ~object_name:"stack" ~implementation:"lock-free"
    ~num_ops ~seconds:stack_lf.seconds ~throughput:stack_lf.throughput ~penalty_pct:0.0;

  let stack_wf =
    benchmark_stack
      ~num_threads
      ~total_ops:(num_ops / 2)
      ~make_universal:WFUniversalStack.create
      ~push:(fun obj value -> WFUniversalStack.apply obj (Sequential.SequentialStack.Push value))
      ~pop:(fun obj -> WFUniversalStack.apply obj Sequential.SequentialStack.Pop)
  in
  let stack_penalty_pct =
    if stack_lf.throughput = 0.0 then 0.0
    else ((stack_lf.throughput -. stack_wf.throughput) /. stack_lf.throughput) *. 100.0
  in
  append_csv_row oc ~object_name:"stack" ~implementation:"wait-free"
    ~num_ops ~seconds:stack_wf.seconds ~throughput:stack_wf.throughput
    ~penalty_pct:stack_penalty_pct;

  let queue_lf =
    benchmark_queue
      ~num_threads
      ~total_ops:(num_ops / 2)
      ~make_universal:LFUniversalQueue.create
      ~enq:(fun obj value -> LFUniversalQueue.apply obj (Sequential.SequentialQueue.Enqueue value))
      ~deq:(fun obj -> LFUniversalQueue.apply obj Sequential.SequentialQueue.Dequeue)
  in
  append_csv_row oc ~object_name:"queue" ~implementation:"lock-free"
    ~num_ops ~seconds:queue_lf.seconds ~throughput:queue_lf.throughput ~penalty_pct:0.0;

  let queue_wf =
    benchmark_queue
      ~num_threads
      ~total_ops:(num_ops / 2)
      ~make_universal:WFUniversalQueue.create
      ~enq:(fun obj value -> WFUniversalQueue.apply obj (Sequential.SequentialQueue.Enqueue value))
      ~deq:(fun obj -> WFUniversalQueue.apply obj Sequential.SequentialQueue.Dequeue)
  in
  let queue_penalty_pct =
    if queue_lf.throughput = 0.0 then 0.0
    else ((queue_lf.throughput -. queue_wf.throughput) /. queue_lf.throughput) *. 100.0
  in
  append_csv_row oc ~object_name:"queue" ~implementation:"wait-free"
    ~num_ops ~seconds:queue_wf.seconds ~throughput:queue_wf.throughput
    ~penalty_pct:queue_penalty_pct;

  close_out oc;
  output_file

let () =
  let num_ops =
    if Array.length Sys.argv >= 2 then int_of_string Sys.argv.(1) else 2_000
  in
  Printf.printf "Running benchmark with %d total operations...\n" num_ops;
  let output_file = benchmark ~num_ops in
  Printf.printf "Benchmark complete. Results written to %s\n" output_file