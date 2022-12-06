open Import

(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2020                              *)
(* Written by: Emilio Jesús Gallego Arias *)

(* Build rules for Coq's .v -> .vo files *)

open Coq_stanza

module Bootstrap : sig
  type t =
    | No_boot  (** Coq's stdlib is installed globally *)
    | Bootstrap of Coq_lib.t
        (** Coq's stdlib is in scope of the composed build *)
    | Bootstrap_prelude  (** We are compiling the prelude itself *)
end

val coqdoc_directory_targets :
  dir:Path.Build.t -> Coq_stanza.Theory.t -> Loc.t Path.Build.Map.t

val setup_rules :
     sctx:Super_context.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> Theory.t
  -> unit Memo.t

val install_rules :
     sctx:Super_context.t
  -> dir:Path.Build.t
  -> Theory.t
  -> Install.Entry.Sourced.t list Memo.t

val setup_coqpp_rules :
  sctx:Super_context.t -> dir:Path.Build.t -> Coqpp.t -> unit Memo.t

val setup_extraction_rules :
     sctx:Super_context.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> Extraction.t
  -> unit Memo.t

(** [deps_of ~dir ~boot_type m] produces an action builder that can be run to
    build all dependencies of the Coq module [m]. *)
val deps_of :
     dir:Path.Build.t
  -> boot_type:Bootstrap.t Resolve.Memo.t
  -> Coq_module.t
  -> unit Dune_engine.Action_builder.t

val coqtop_args_theory :
     sctx:Super_context.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> Theory.t
  -> Coq_module.t
  -> ('a Command.Args.t list Action_builder.t * Bootstrap.t) Memo.t

val coqtop_args_extraction :
     sctx:Super_context.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> Extraction.t
  -> ('a Command.Args.t list Action_builder.t * Bootstrap.t) Memo.t
