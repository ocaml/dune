(** Actions executed by dune may produce trace events in a designated
    directory. These are collected by dune and inserted into its trace file. *)

open Import

type t

val add_to_env : t -> Env.t -> Env.t
val create : Dune_digest.t -> t
val collect : t -> unit Fiber.t
