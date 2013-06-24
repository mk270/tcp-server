
(*
  Tcp_server, a TCP server generator for OCaml Lwt

  Copyright (C) 2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache License v2.0
*)

open Unix

module Tcp_server : sig

	module Connection_id : sig
		type t
	end

	type callback = 
			Connection_id.t -> Lwt_io.input_channel -> 
			Lwt_io.output_channel -> unit Lwt.t

	val create : sockaddr -> callback -> unit Lwt.t
end
