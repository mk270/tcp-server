
(*
  Tcp_server, a TCP server generator for OCaml Lwt

  Copyright (C) 2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache License v2.0
*)

open Tcp_server
open Lwt

let () = 
	let write_and_flush ch data =
		Lwt_io.write ch data >>=
			fun () -> Lwt_io.flush ch
	in
	let respond data =
		print_endline data;
		match data with
			| "help" -> "no help available\r\n"
			| _ -> "what?\r\n"
	in
	let io_loop input output =
		Lwt_io.read_line input >|=
		respond >>=
		write_and_flush output
	in
	let cb input output = 
		write_and_flush output "Hello?\r\n" >>=
		fun () -> while_lwt true do io_loop input output done
	in
	let sa = Unix.ADDR_INET (Unix.inet_addr_any, 2092) in
	let serv = Tcp_server.create sa cb in
		Lwt_main.run serv
