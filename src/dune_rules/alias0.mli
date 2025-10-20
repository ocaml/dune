(** Standard set of aliases in the rules. *)

(* this module contains the trailing zero not to collide with [Dune_engine.Alias] *)

module Name := Dune_engine.Alias.Name

val fmt : Name.t
val doc : Name.t
val doc_json : Name.t
val lint : Name.t
val private_doc : Name.t
val doc_new : Name.t
val check : Name.t
val ocaml_index : Name.t
val install : Name.t
val pkg_install : Name.t
val pkg_lock : Name.t
val runtest : Name.t
val empty : Name.t
val all : Name.t
val default : Name.t
val is_standard : Name.t -> bool
val register_as_standard : Name.t -> unit
