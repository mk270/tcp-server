
open Tcp_server
open Lwt

let () = 
	let cb input output = 
		Lwt_io.write output "Hello\r\n" >>=
		fun () -> Lwt_io.flush output >>=
		fun () -> Lwt.return () 
	in
	let sa = Unix.ADDR_INET (Unix.inet_addr_any, 2092) in
	let serv = Tcp_server.create sa cb in
		Lwt_main.run serv
