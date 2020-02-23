(** Handling of coq source files *)
open Stdune

type t

val empty : t

(** Coq modules of library [name] is the Coq library name. *)
val library : t -> name:Coq_lib_name.t -> Coq_module.t list

val of_dir :
     Stanza.t list Dir_with_dune.t
  -> subdirs:(Path.Build.t * string list * String.Set.t) list
  -> t
