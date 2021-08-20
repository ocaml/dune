open! Dune_engine

(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2020                              *)
(* Written by: Emilio Jesús Gallego Arias *)

(* Build rules for Coq's .v -> .vo files *)

open! Stdune
open Coq_stanza

val setup_rules :
     sctx:Super_context.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> Theory.t
  -> Action.Full.t Action_builder.With_targets.t list Memo.Build.t

val install_rules :
     sctx:Super_context.t
  -> dir:Path.Build.t
  -> Theory.t
  -> Install.Entry.Sourced.t list Memo.Build.t

val coqpp_rules :
     sctx:Super_context.t
  -> dir:Path.Build.t
  -> Coqpp.t
  -> Action.Full.t Action_builder.With_targets.t list Memo.Build.t

val extraction_rules :
     sctx:Super_context.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> Extraction.t
  -> Action.Full.t Action_builder.With_targets.t list Memo.Build.t
