open Import

(** This module loads and validates foreign sources from directories. *)

type t

val empty : t
val for_lib : t -> name:Lib_name.t -> Foreign.Sources.t
val for_archive : t -> archive_name:Foreign.Archive.Name.t -> Foreign.Sources.t
val for_exes : t -> first_exe:string -> Foreign.Sources.t

val make
  :  Stanza.t list
  -> dune_version:Syntax.Version.t
  -> dirs:Source_file_dir.t list
  -> t
