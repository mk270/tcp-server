
(*
  Tcp_server, a TCP server generator for OCaml Lwt

  Copyright (C) 2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache License v2.0
*)

open Unix
	
module Connection_id : sig
	type t
end

type callback = 
		Connection_id.t -> Lwt_io.input_channel -> 
		Lwt_io.output_channel -> unit Lwt.t

val create : sockaddr -> callback -> unit Lwt.t

val enqueue : Connection_id.t -> string -> unit
val enqueue_all : (Connection_id.t * string) list -> unit

val all_connection_ids : unit -> Connection_id.t list


