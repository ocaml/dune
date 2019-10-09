(** This module loads and validates foreign sources from directories. *)

type t

val empty : t

val for_lib : t -> name:Lib_name.t -> Foreign.Sources.t

val for_foreign_lib : t -> archive_name:string -> Foreign.Sources.t

val for_exes : t -> first_exe:string -> Foreign.Sources.t

(** [make stanzas ~c_sources] loads and validates foreign sources. *)
val make :
  Stanza.t list Dir_with_dune.t -> object_map:Foreign.Object_map.t -> t
