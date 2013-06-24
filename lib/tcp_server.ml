
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
		messages : string Queue.t
	}

	type callback = 
			Connection_id.t -> Lwt_io.input_channel -> 
			Lwt_io.output_channel -> unit Lwt.t

	let connections : (Connection_id.t, connection) Hashtbl.t = Hashtbl.create 5

	let make_connection c =
		let conn_id = Connection_id.create () in
			Hashtbl.replace connections conn_id c;
			conn_id

	let enqueue conn_id msg =
		let conn = Hashtbl.find connections conn_id in
			Queue.add msg conn.messages

	let flush conn_id =
		let conn = Hashtbl.find connections conn_id in
		let elts = List.rev (Queue.fold (fun a b -> b :: a) [] conn.messages) in
			Queue.clear conn.messages;
			elts

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
		let conn_id = make_connection { 
			fds = connection;
			messages = Queue.create ();
		} in
		let guarded_handler () = 
			lwt_guard (fun () -> handler conn_id input output) in
		let _ =	guarded_handler () >>= fun () -> shutdown () in
			Lwt.return ()

	let loop_forever thunk =
		while_lwt true do thunk () done
			
	let create address callback =
		let skt = create_listener address in
			loop_forever (fun () -> 
				Lwt_unix.accept skt >>= handle_accept callback)

end
