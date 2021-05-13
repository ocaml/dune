(** Auxiliary functions for working with Unix errors. *)

type t = Unix.error

val equal : t -> t -> bool
