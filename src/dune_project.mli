(** dune-project files *)

open Import

type t =
  { name : string
  }

val load : dir:Path.t -> t

(** "dune-project" *)
val filename : string
