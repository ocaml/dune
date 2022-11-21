open Import

(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2020                              *)
(* Written by: Emilio Jesús Gallego Arias *)

module Bootstrap : sig
  type t =
    | No_boot  (** Coq's stdlib is installed globally *)
    | Bootstrap of Coq_lib.t
        (** Coq's stdlib is in scope of the composed build *)
    | Bootstrap_prelude  (** We are compiling the prelude itself *)
end

val boot_type :
     dir:Path.Build.t
  -> use_stdlib:bool
  -> wrapper_name:string
  -> Coq_module.t
  -> Bootstrap.t Action_builder.t

(** [deps_of ~dir ~boot_type m] produces an action builder that can be run to
    build all dependencies of the Coq module [m]. *)
val deps_of :
     dir:Path.Build.t
  -> boot_type:Bootstrap.t Action_builder.t
  -> Coq_module.t
  -> unit Dune_engine.Action_builder.t

val coqdoc_directory_targets :
  dir:Path.Build.t -> Coq_stanza.Theory.t -> Loc.t Path.Build.Map.t

(** ** Rules for Coq stanzas *)

(**coq.theory stanza rules *)
val setup_theory_rules :
     sctx:Super_context.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> Coq_stanza.Theory.t
  -> unit Memo.t

(** Arguments for `dune coq top` in presence of coq.theory. *)
val coqtop_args_theory :
     sctx:Super_context.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> Coq_stanza.Theory.t
  -> Coq_module.t
  -> 'a Command.Args.t list Action_builder.t

(** coq.extraction stanza rules *)
val setup_extraction_rules :
     sctx:Super_context.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> Coq_stanza.Extraction.t
  -> unit Memo.t

(** Arguments for `dune coq top` in presence of coq.extraction. *)
val coqtop_args_extraction :
     sctx:Super_context.t
  -> dir:Path.Build.t
  -> Coq_stanza.Extraction.t
  -> Coq_module.t
  -> 'a Command.Args.t list Action_builder.t

val setup_coqpp_rules :
  sctx:Super_context.t -> dir:Path.Build.t -> Coq_stanza.Coqpp.t -> unit Memo.t

val install_rules :
     sctx:Super_context.t
  -> dir:Path.Build.t
  -> Coq_stanza.Theory.t
  -> Install.Entry.Sourced.t list Memo.t
