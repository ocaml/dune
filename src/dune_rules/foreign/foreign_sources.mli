open Import

(** This module loads and validates foreign sources from directories. *)

type t

val empty : t
val for_lib : t -> name:Lib_name.t -> Foreign_source_files.t
val for_archive : t -> archive_name:Foreign_archive.Name.t -> Foreign_source_files.t
val for_exes : t -> first_exe:string -> Foreign_source_files.t

val make
  :  Stanza.t list
  -> dir:Path.Build.t
  -> dune_version:Syntax.Version.t
  -> dirs:Source_file_dir.t list
  -> t Memo.t
