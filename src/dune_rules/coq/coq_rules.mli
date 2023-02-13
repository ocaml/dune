open Import

(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2020                              *)
(* Written by: Emilio Jesús Gallego Arias *)

module Dep_map : Map.S with type key := Path.t

(** [get_dep_map] produces a dep map for a theory *)
val get_dep_map :
     dir:Path.Build.t
  -> use_stdlib:bool
  -> wrapper_name:string
  -> Coq_module.t
  -> Path.t list Dep_map.t Dune_engine.Action_builder.t

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
  -> 'a Command.Args.t list Memo.t

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
  -> 'a Command.Args.t list Memo.t

val setup_coqpp_rules :
  sctx:Super_context.t -> dir:Path.Build.t -> Coq_stanza.Coqpp.t -> unit Memo.t

val install_rules :
     sctx:Super_context.t
  -> dir:Path.Build.t
  -> Coq_stanza.Theory.t
  -> Install.Entry.Sourced.t list Memo.t
