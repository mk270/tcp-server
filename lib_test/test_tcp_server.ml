
(*
  Tcp_server, a TCP server generator for OCaml Lwt

  Copyright (C) 2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache License v2.0
*)

open Tcp_server
open Lwt

	let write_and_flush ch data =
		Lwt_io.write ch data >>=
			fun () -> Lwt_io.flush ch

	let respond data =
		print_endline data;
		match data with
			| "help" -> "no help available\r\n"
			| _ -> "what?\r\n"

	let io_loop connection_id input output =
		Lwt_io.read_line input >|=
		(fun data -> let response = respond data in
						 Tcp_server.enqueue connection_id response) >>=
		fun () -> let msgs = Tcp_server.flush connection_id in
					  Lwt_list.iter_s (write_and_flush output) msgs


	let cb connection_id input output = 
		write_and_flush output "Hello?\r\n" >>=
		fun () -> while_lwt true do io_loop connection_id input output done

	let sa = Unix.ADDR_INET (Unix.inet_addr_any, 2092)

let () = 
	let serv = Tcp_server.create sa cb in
		Lwt_main.run serv
