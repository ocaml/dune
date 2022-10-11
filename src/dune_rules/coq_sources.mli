(** Handling of coq source files *)
open Import

open Coq_stanza

type t

val empty : t

(** Coq modules of library [name] is the Coq library name. *)
val library : t -> name:Coq_lib_name.t -> Coq_module.t list

val directories : t -> name:Coq_lib_name.t -> Path.Build.t list

val extract : t -> Extraction.t -> Coq_module.t

val of_dir :
     Stanza.t list
  -> dir:Path.Build.t
  -> include_subdirs:Loc.t * Dune_file.Include_subdirs.t
  -> dirs:(Path.Build.t * string list * String.Set.t) list
  -> t

(** [find_module ~source t] finds a Coq library name and module corresponding to
    file [source], if there is one. *)
val find_module :
  source:Path.Build.t -> t -> (Coq_lib_name.t * Coq_module.t) option

val lookup_module :
     t
  -> Coq_module.t
  -> [ `Theory of Theory.t | `Extraction of Extraction.t ] option

val mlg_files :
     sctx:Super_context.t
  -> dir:Path.Build.t
  -> modules:Ordered_set_lang.t
  -> Path.Build.t list Memo.t

module Coqffi : sig
  val lib : dir:Path.Build.t -> library:Loc.t * Lib_name.t -> Lib.t Memo.t

  (** The target of the coqffi rule given a module name. *)
  val target_of : dir:Path.Build.t -> Module_name.t -> Path.Build.t

  (** Checks if the give list of module names are valid modules that can be
      found. *)
  val modules_of :
       loc:Loc.t
    -> lib:Lib.t
    -> modules:Module_name.t list
    -> ml_sources:Ml_sources.t Memo.t
    -> Module.t list Memo.t

  val targets : dir:Path.Build.t -> Ffi.t -> Path.Build.t list
end
