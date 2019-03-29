(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(*     Written by: Emilio Jesús Gallego Arias  *)

(* Build rules for Coq's .v -> .vo files       *)

open! Stdune

val setup_rules
  :  sctx:Super_context.t
  -> dir:Path.t
  -> dir_contents:Dir_contents.t
  -> Dune_file.Coq.t
  -> (unit, Action.t) Build.t list
