open Stdune

type t = {root: Path.t}

let make root : t = {root}

let peer_name = function
  | Unix.ADDR_UNIX _ ->
      failwith "got a Unix socket connection on our TCP socket ?"
  | Unix.ADDR_INET (addr, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr addr) port

let run _ =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let finally () = Unix.close sock
  and f () =
    Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_of_string "0.0.0.0", 4242)) ;
    Unix.listen sock 1024 ;
    while true do
      let fd, peer = Unix.accept sock in
      print_endline (peer_name peer) ;
      ignore (fd, peer)
    done
  in
  Exn.protect ~f ~finally
