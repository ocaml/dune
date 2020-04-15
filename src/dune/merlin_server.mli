(** Merlin config server *)

val start : unit -> unit
(** Once started the server will wait for commands on stdin, read the requested
    merlin dot file and return its content on stdout. *)
