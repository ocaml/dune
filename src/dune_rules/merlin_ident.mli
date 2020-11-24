(** Merlin identifiers allow the unique identification of a merlin file attached
    to a specific [library] or [executable] stanza. *)
type t

val for_lib : Dune_engine.Lib_name.t -> t

val for_exes : names:string list -> t

val to_string : t -> string
