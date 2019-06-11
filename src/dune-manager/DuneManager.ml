open Stdune

type t = {root: Path.t}

let make root : t = {root}

let peer_name = function
  | Unix.ADDR_UNIX _ ->
      failwith "got a Unix socket connection on our TCP socket ?"
  | Unix.ADDR_INET (addr, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr addr) port

module Parser = Csexp.Parser (Csexp.ChannelStream)

let run _ =
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let finally () = Unix.close sock
  and f () =
    Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_of_string "0.0.0.0", 4242)) ;
    Unix.listen sock 1024 ;
    while true do
      let fd, peer = Unix.accept sock in
      try
        Printf.printf "accept client: %s\n" (peer_name peer) ;
        let input = Csexp.ChannelStream.make (Unix.in_channel_of_descr fd) in
        while true do
          (* Skip toplevel newlines, for easy netcat interaction *)
          while Csexp.ChannelStream.peek_byte input = int_of_char '\n' do
            ignore (Csexp.ChannelStream.input_byte input)
          done ;
          let cmd = Parser.parse_stream input in
          print_endline (Sexp.to_string cmd)
        done
      with
      | End_of_file ->
          ()
      | Csexp.Parse_error msg ->
          Printf.printf "Canonical SExp parse error: %s\n" msg
    done
  in
  Exn.protect ~f ~finally
