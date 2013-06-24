
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

let respond = function
	| "help" -> "no help available\r\n"
	| "shout" ->
		let ids = Tcp_server.all_connection_ids () in
			List.iter (fun id -> Tcp_server.enqueue id "HI!") ids;
			"broadcast sent"
	| _ -> "what?\r\n"
			
let io_loop connection_id input output =
	Lwt_io.read_line input >|=
	(fun data -> let response = respond data in
					 Tcp_server.enqueue connection_id response)

let cb connection_id input output = 
	Tcp_server.enqueue connection_id "Hello?\r\n";
    while_lwt true do io_loop connection_id input output done

let sa = Unix.ADDR_INET (Unix.inet_addr_any, 2092)

let () = 
	let serv = Tcp_server.create sa cb in
		Lwt_main.run serv
