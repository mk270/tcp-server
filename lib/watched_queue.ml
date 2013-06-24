
(*
  Tcp_server, a TCP server generator for OCaml Lwt

  Copyright (C) 2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache License v2.0
*)

open Lwt

	type t = {
		condition : unit Lwt_condition.t;
		queue : string Queue.t
	}
		
	let create () = {
		condition = Lwt_condition.create ();
		queue = Queue.create ();
	}

	let enqueue wq s =
		Queue.add s wq.queue;
		Lwt_condition.signal wq.condition ()

	let dequeue wq =
		let rcons a b = b :: a in
		Lwt_condition.wait wq.condition >>=
		fun () ->
			let elts = List.rev (Queue.fold rcons [] wq.queue) in
				Queue.clear wq.queue;
				return elts		
		
