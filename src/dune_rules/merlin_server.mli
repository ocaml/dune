(** Merlin config server *)
open! Dune_engine

val dump : string -> unit

val dump_dot_merlin : string -> unit

(** Once started the server will wait for commands on stdin, read the requested
    merlin dot file and return its content on stdout. The server will halt when
    reiceving EOF of a bad csexp. *)
val start : unit -> unit
