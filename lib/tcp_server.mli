open Unix

module Tcp_server : sig
	val create : 
		sockaddr -> 
		(Lwt_io.input_channel -> Lwt_io.output_channel -> unit Lwt.t) -> 
		unit Lwt.t
end
