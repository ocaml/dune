(** This module loads and validates foreign sources from directories. *)

type t

val empty : t

val for_lib : t -> name:Lib_name.t -> Foreign.Sources.t

val for_archive : t -> archive_name:Foreign.Archive.Name.t -> Foreign.Sources.t

val for_exes : t -> first_exe:string -> Foreign.Sources.t

(** [make stanzas ~sources ~ext_obj] loads and validates foreign sources. We use
    [ext_obj] only for nicer error messages. *)
val make :
     Stanza.t list Dir_with_dune.t
  -> sources:Foreign.Sources.Unresolved.t
  -> ext_obj:string
  -> t
