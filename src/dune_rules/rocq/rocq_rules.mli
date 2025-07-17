(***********************************************)
(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2019-2024                         *)
(* (c) Emilio J. Gallego Arias 2024-2025       *)
(* (c) CNRS 2025                               *)
(***********************************************)
(* Written by: Ali Caglayan                    *)
(* Written by: Emilio Jesús Gallego Arias      *)
(* Written by: Rudi Grinberg                   *)
(* Written by: Rodolphe Lepigre                *)
(***********************************************)

open Import

(** [deps_of ~dir ~use_stdlib ~wrapper_name rocq_module] action that builds the
    deps of [rocq_module] *)
val deps_of
  :  dir:Path.Build.t
  -> use_stdlib:bool
  -> wrapper_name:string
  -> mode:Rocq_mode.t
  -> Rocq_module.t
  -> unit Dune_engine.Action_builder.t

(** ** Rules for Rocq stanzas *)

(** [rocq.theory] stanza rules *)
val setup_theory_rules
  :  sctx:Super_context.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> Rocq_stanza.Theory.t
  -> unit Memo.t

(** Arguments for `dune rocq top` in presence of rocq.theory. *)
val rocqtop_args_theory
  :  sctx:Super_context.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> Rocq_stanza.Theory.t
  -> Rocq_module.t
  -> 'a Command.Args.t list Memo.t

(** rocq.extraction stanza rules *)
val setup_extraction_rules
  :  sctx:Super_context.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> Rocq_stanza.Extraction.t
  -> unit Memo.t

(** Arguments for `dune rocq top` in presence of rocq.extraction. *)
val rocqtop_args_extraction
  :  sctx:Super_context.t
  -> dir:Path.Build.t
  -> Rocq_stanza.Extraction.t
  -> Rocq_module.t
  -> 'a Command.Args.t list Memo.t

(** Rules for .mlg to .ml transformation *)
val setup_rocqpp_rules
  :  sctx:Super_context.t
  -> dir:Path.Build.t
  -> Rocq_stanza.Rocqpp.t
  -> unit Memo.t

(** Install rules for a Rocq package  *)
val install_rules
  :  sctx:Super_context.t
  -> dir:Path.Build.t
  -> Rocq_stanza.Theory.t
  -> Install.Entry.Sourced.t list Memo.t

(** Compute the effective Rocq enviroment *)
val rocq_env : dir:Path.Build.t -> Rocq_flags.t Action_builder.t
