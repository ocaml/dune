open! Dune_engine

(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* Written by: Emilio Jesús Gallego Arias *)

(* Build rules for Coq's .v -> .vo files *)

open! Stdune
open Coq_stanza

val setup_rules :
     sctx:Super_context.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> Theory.t
  -> Action.t Build.With_targets.t list

val install_rules :
     sctx:Super_context.t
  -> dir:Path.Build.t
  -> Theory.t
  -> (Loc.t option * Path.Build.t Install.Entry.t) list

val coqpp_rules :
     sctx:Super_context.t
  -> dir:Path.Build.t
  -> Coqpp.t
  -> Action.t Build.With_targets.t list

val extraction_rules :
     sctx:Super_context.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> Extraction.t
  -> Action.t Build.With_targets.t list
