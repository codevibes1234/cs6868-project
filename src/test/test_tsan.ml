open Src
open Sequential

let num_threads = 8

module type SeqLike = sig
	type 'a state
	type 'a op
	val apply : 'a op -> 'a state -> 'a state * 'a option
	val empty : unit -> 'a state
end

module MakeLF (Seq : SeqLike) = struct
	type 'a t = ('a Seq.state * 'a option, 'a option) LFUniversal.t

	let create = LFUniversal.create

	let apply obj op =
		let invoc (state, _) =
			let (next_state, result) = Seq.apply op state in
			((next_state, result), result)
		in
		let initial_obj = (Seq.empty (), None) in
		let (_new_obj, result) = LFUniversal.apply obj initial_obj invoc in
		result
end

module MakeWF (Seq : SeqLike) = struct
	type 'a t = ('a Seq.state * 'a option, 'a option) WFUniversal.t

	let create = WFUniversal.create

	let apply obj op =
		let invoc (state, _) =
			let (next_state, result) = Seq.apply op state in
			((next_state, result), result)
		in
		let initial_obj = (Seq.empty (), None) in
		let (_new_obj, result) = WFUniversal.apply obj initial_obj invoc in
		result
end

module StackSeq = struct
	type 'a state = 'a SequentialStack.state
	type 'a op = 'a SequentialStack.op
	let apply = SequentialStack.apply
	let empty () = SequentialStack.empty
end

module QueueSeq = struct
	type 'a state = 'a SequentialQueue.state
	type 'a op = 'a SequentialQueue.op
	let apply = SequentialQueue.apply
	let empty () = SequentialQueue.empty
end

module SortedListSeq = struct
	type 'a state = 'a SequentialSortedList.state
	type 'a op = 'a SequentialSortedList.op
	let apply op state = SequentialSortedList.apply state op
	let empty () = SequentialSortedList.empty
end

module SkipListSeq = struct
	type 'a state = 'a SequentialSkipList.state
	type 'a op = 'a SequentialSkipList.op
	let empty () = SequentialSkipList.empty ()
	let apply op state = SequentialSkipList.apply op state
end

module BstSeq = struct
	type 'a state = 'a SequentialBst.state
	type 'a op = 'a SequentialBst.op
	let empty () = SequentialBst.empty
	let apply op state = SequentialBst.apply state op
end

module LFStack = MakeLF(StackSeq)
module WFStack = MakeWF(StackSeq)
module LFQueue = MakeLF(QueueSeq)
module WFQueue = MakeWF(QueueSeq)
module LFList = MakeLF(SortedListSeq)
module WFList = MakeWF(SortedListSeq)
module LFSkipList = MakeLF(SkipListSeq)
module WFSkipList = MakeWF(SkipListSeq)
module LFBst = MakeLF(BstSeq)
module WFBst = MakeWF(BstSeq)

let run_domains n f =
	let domains = Array.init n (fun id -> Domain.spawn (fun () -> f id)) in
	Array.iter Domain.join domains

let burn_iters = 50_000

let test_stack
	name
	(type t)
	(create : int -> t)
	(apply : t -> int SequentialStack.op -> int option) =
	Printf.printf "[TSAN] %s stack...\n%!" name;
	let s = create num_threads in
	run_domains num_threads (fun id ->
		for i = 1 to burn_iters do
			let v = (id * burn_iters) + i in
			ignore (apply s (SequentialStack.Push v));
			if i mod 3 = 0 then ignore (apply s SequentialStack.Pop)
		done
	);
	ignore (apply s SequentialStack.Pop)

let test_queue
	name
	(type t)
	(create : int -> t)
	(apply : t -> int SequentialQueue.op -> int option) =
	Printf.printf "[TSAN] %s queue...\n%!" name;
	let q = create num_threads in
	run_domains num_threads (fun id ->
		for i = 1 to burn_iters do
			let v = (id * burn_iters) + i in
			ignore (apply q (SequentialQueue.Enqueue v));
			if i mod 2 = 0 then ignore (apply q SequentialQueue.Dequeue)
		done
	);
	ignore (apply q SequentialQueue.Dequeue)

let test_list
	name
	(type t)
	(create : int -> t)
	(apply : t -> int SequentialSortedList.op -> int option) =
	Printf.printf "[TSAN] %s sorted list...\n%!" name;
	let l = create num_threads in
	run_domains num_threads (fun id ->
		let base = id * burn_iters in
		for i = 1 to burn_iters do
			let v = base + i in
			match i mod 3 with
			| 0 -> ignore (apply l (SequentialSortedList.Insert v))
			| 1 -> ignore (apply l (SequentialSortedList.Remove v))
			| _ -> ignore (apply l (SequentialSortedList.Contains v))
		done
	)

let test_skiplist
	name
	(type t)
	(create : int -> t)
	(apply : t -> int SequentialSkipList.op -> int option) =
	Printf.printf "[TSAN] %s skip list...\n%!" name;
	let l = create num_threads in
	run_domains num_threads (fun id ->
		let base = id * burn_iters in
		for i = 1 to burn_iters do
			let v = base + i in
			match i mod 3 with
			| 0 -> ignore (apply l (SequentialSkipList.Insert v))
			| 1 -> ignore (apply l (SequentialSkipList.Remove v))
			| _ -> ignore (apply l (SequentialSkipList.Contains v))
		done
	)

let test_bst
	name
	(type t)
	(create : int -> t)
	(apply : t -> int SequentialBst.op -> int option) =
	Printf.printf "[TSAN] %s bst...\n%!" name;
	let t = create num_threads in
	run_domains num_threads (fun id ->
		let base = id * burn_iters in
		for i = 1 to burn_iters do
			let v = base + i in
			match i mod 3 with
			| 0 -> ignore (apply t (SequentialBst.Insert v))
			| 1 -> ignore (apply t (SequentialBst.Remove v))
			| _ -> ignore (apply t (SequentialBst.Contains v))
		done
	)

let () =
	Random.self_init ();
	test_stack "LF" LFStack.create LFStack.apply;
	test_stack "WF" WFStack.create WFStack.apply;

	test_queue "LF" LFQueue.create LFQueue.apply;
	test_queue "WF" WFQueue.create WFQueue.apply;

	test_list "LF" LFList.create LFList.apply;
	test_list "WF" WFList.create WFList.apply;

	test_skiplist "LF" LFSkipList.create LFSkipList.apply;
	test_skiplist "WF" WFSkipList.create WFSkipList.apply;

	test_bst "LF" LFBst.create LFBst.apply;
	test_bst "WF" WFBst.create WFBst.apply;

	Printf.printf "[TSAN] done\n%!"
