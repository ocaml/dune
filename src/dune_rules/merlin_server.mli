(** Merlin config server *)
open! Dune_engine

val dump : string -> unit Fiber.t

val dump_dot_merlin : string -> unit Fiber.t

(** Once started the server will wait for commands on stdin, read the requested
    merlin dot file and return its content on stdout. The server will halt when
    receiving EOF of a bad csexp. *)
val start : unit -> unit Fiber.t
