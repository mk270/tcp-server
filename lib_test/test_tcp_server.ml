
(*
  Tcp_server, a TCP server generator for OCaml Lwt

  Copyright (C) 2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache License v2.0
*)

open Tcp_server
open Lwt

let io_loop connection_id input output =
	let respond = function
		| "help" -> [(connection_id, "no help available\r\n")]
		| "shout" ->
			let ids = Tcp_server.all_connection_ids () in
				List.map (fun id -> (id, "HI!\r\n")) ids
		| _ -> [(connection_id, "what?\r\n")]
	in

	Lwt_io.read_line input >|=
	respond >|=
	Tcp_server.enqueue_all

let cb connection_id input output = 
	Tcp_server.enqueue connection_id "Hello?\r\n";
    while_lwt true do io_loop connection_id input output done

let sa = Unix.ADDR_INET (Unix.inet_addr_any, 2092)

let () = 
	let serv = Tcp_server.create sa cb in
		Lwt_main.run serv
