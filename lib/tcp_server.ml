
(*
  Tcp_server, a TCP server generator for OCaml Lwt

  Copyright (C) 2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache License v2.0
*)

open Unix
open Lwt
open Lwt_unix

module Tcp_server = struct

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
		let close ch = try_lwt Lwt_io.close ch with _ -> return () in
		let shutdown () = Lwt.join [close input; close output;] in
		let _ =	handler input output >>= fun () -> shutdown () in
			Lwt.return ()

	let loop_forever thunk =
		while_lwt true do thunk () done
			
	let create address callback =
		let skt = create_listener address in
			loop_forever (fun () -> 
				Lwt_unix.accept skt >>= handle_accept callback)

end
