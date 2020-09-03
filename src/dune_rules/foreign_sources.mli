open! Dune_engine
open Import

(** This module loads and validates foreign sources from directories. *)

type t

val empty : t

val for_lib : t -> name:Lib_name.t -> Foreign.Sources.t

val for_archive : t -> archive_name:Foreign.Archive.Name.t -> Foreign.Sources.t

val for_exes : t -> first_exe:string -> Foreign.Sources.t

val make :
     Stanza.t list Dir_with_dune.t
  -> include_subdirs:Loc.t * Dune_file.Include_subdirs.t
  -> lib_config:Lib_config.t
  -> dirs:(Path.Build.t * 'a * String.Set.t) list
  -> t
