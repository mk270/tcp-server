
(*
  Tcp_server, a TCP server generator for OCaml Lwt

  Copyright (C) 2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache License v2.0
*)

open Unix
open Lwt
open Lwt_unix

let lwt_guard f = try_lwt f () with _ -> return ()

module Watched_queue : sig
	type t

	val create : unit -> t
	val enqueue : t -> string -> unit
	val dequeue : t -> string list Lwt.t
end = struct
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
		
end


module Tcp_server = struct

	module Connection_id : sig
		type t
		val create : unit -> t
	end = struct
		type t = int

		let counter = ref 0

		let create () =
			incr counter;
			!counter
    end

	type connection = {
		fds : Lwt_unix.file_descr;
		mailbox : Watched_queue.t
	}

	type callback = 
			Connection_id.t -> Lwt_io.input_channel -> 
			Lwt_io.output_channel -> unit Lwt.t

	let connections : (Connection_id.t, connection) Hashtbl.t = Hashtbl.create 5

	let loop_forever thunk =
		while_lwt true do thunk () done

	let all_connection_ids () =
		Hashtbl.fold (fun k v acc -> k :: acc) connections []

	let make_connection c =
		let conn_id = Connection_id.create () in
			Hashtbl.replace connections conn_id c;
			conn_id

	let enqueue conn_id msg =
		let conn = Hashtbl.find connections conn_id in
			Watched_queue.enqueue conn.mailbox msg

	let write_and_flush ch data =
		Lwt_io.write ch data >>=
			fun () -> Lwt_io.flush ch

	let send_output output mailbox =
		loop_forever (fun () ->
			Watched_queue.dequeue mailbox >>=
			Lwt_list.iter_s (write_and_flush output)
		)

	let create_listener sa =
		let skt = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
			Lwt_unix.setsockopt skt SO_REUSEADDR true;
			Lwt_unix.bind skt sa;
			Lwt_unix.listen skt 5;
			skt

	let handle_accept handler new_socket =
		let connection, _ = new_socket in
		let input = Lwt_io.of_fd Lwt_io.input connection in
		let output = Lwt_io.of_fd Lwt_io.output connection in
		let close ch = lwt_guard (fun () -> Lwt_io.close ch) in
		let shutdown () = Lwt.join [close input; close output;] in
		let mailbox = Watched_queue.create () in
		let conn_id = make_connection { 
			fds = connection;
			mailbox = mailbox;
		} in
		let guarded_handler () = 
			lwt_guard (fun () -> 
				let reader = handler conn_id input output in
				let writer = send_output output mailbox in
					Lwt.pick [reader; writer]
			) 
		in
		let _ =	guarded_handler () >>= fun () -> shutdown () in
			Lwt.return ()
			
	let create address callback =
		let skt = create_listener address in
			loop_forever (fun () -> 
				Lwt_unix.accept skt >>= handle_accept callback)

end
