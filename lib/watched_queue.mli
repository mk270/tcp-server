
(*
  Tcp_server, a TCP server generator for OCaml Lwt

  Copyright (C) 2013  Martin Keegan

  This programme is free software; you may redistribute and/or modify
  it under the terms of the Apache License v2.0
*)

open Lwt

	type t

	val create : unit -> t
	val enqueue : t -> string -> unit
	val dequeue : t -> string list Lwt.t
