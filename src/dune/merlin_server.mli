(** Merlin config server *)

(** Once started the server will wait for commands on stdin, read the requested
    merlin dot file and return its content on stdout. The server will only halt
    when reiving EOF. Bad input will not stop the server but it will answer with
    an error message *)
val start : unit -> unit
