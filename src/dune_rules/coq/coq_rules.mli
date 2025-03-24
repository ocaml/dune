open Import

(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2020-2023                         *)
(* Written by: Ali Caglayan                    *)
(* Written by: Emilio Jesús Gallego Arias      *)
(* Written by: Rudi Grinberg                   *)

(** [deps_of ~dir ~use_stdlib ~wrapper_name coq_module] action that builds the
    deps of [coq_module] *)
val deps_of
  :  dir:Path.Build.t
  -> use_stdlib:bool
  -> wrapper_name:string
  -> mode:Coq_mode.t
  -> coq_lang_version:Dune_sexp.Syntax.Version.t
  -> Coq_module.t
  -> unit Dune_engine.Action_builder.t

(** ** Rules for Coq stanzas *)

(**coq.theory stanza rules *)
val setup_theory_rules
  :  sctx:Super_context.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> Coq_stanza.Theory.t
  -> unit Memo.t

(** Arguments for `dune coq top` in presence of coq.theory. *)
val coqtop_args_theory
  :  sctx:Super_context.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> Coq_stanza.Theory.t
  -> Coq_module.t
  -> 'a Command.Args.t list Memo.t

(** coq.extraction stanza rules *)
val setup_extraction_rules
  :  sctx:Super_context.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> Coq_stanza.Extraction.t
  -> unit Memo.t

(** Arguments for `dune coq top` in presence of coq.extraction. *)
val coqtop_args_extraction
  :  sctx:Super_context.t
  -> dir:Path.Build.t
  -> Coq_stanza.Extraction.t
  -> Coq_module.t
  -> 'a Command.Args.t list Memo.t

val setup_coqpp_rules
  :  sctx:Super_context.t
  -> dir:Path.Build.t
  -> Coq_stanza.Coqpp.t
  -> unit Memo.t

val install_rules
  :  sctx:Super_context.t
  -> dir:Path.Build.t
  -> Coq_stanza.Theory.t
  -> Install.Entry.Sourced.t list Memo.t

val coq_env : dir:Path.Build.t -> Coq_flags.t Action_builder.t
