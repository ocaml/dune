(** Handling of coq source files *)
open Stdune

type t

val empty : t

val library : t -> name:Coq_lib_name.t -> Coq_module.t list
(** Coq modules of library [name] is the Coq library name. *)

val extract : t -> Coq_stanza.Extraction.t -> Coq_module.t

val of_dir :
     Stanza.t list Dir_with_dune.t
  -> include_subdirs:Loc.t * Dune_file.Include_subdirs.t
  -> dirs:(Path.Build.t * string list * String.Set.t) list
  -> t
