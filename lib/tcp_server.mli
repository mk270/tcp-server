
(*
  Tcp_server, a TCP server generator for OCaml Lwt

  Copyright (C) 2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache License v2.0
*)

open Unix

module Tcp_server : sig
	val create : 
		sockaddr -> 
		(Lwt_io.input_channel -> Lwt_io.output_channel -> unit Lwt.t) -> 
		unit Lwt.t
end
