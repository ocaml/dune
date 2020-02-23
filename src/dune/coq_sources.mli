(** Handling of coq source files *)
open Stdune

type t

val empty : t

(** Coq modules of library [name] is the Coq library name. *)
val library : t -> name:Coq_lib_name.t -> Coq_module.t list

val of_dir :
     Stanza.t list Dir_with_dune.t
  -> loc:Loc.t
  -> subdirs:(Path.Build.t * string list * String.Set.t) list
  -> include_subdirs:Dune_file.Include_subdirs.t
  -> t
