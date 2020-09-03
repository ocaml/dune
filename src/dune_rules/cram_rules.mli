(** Rules for setting up cram tests *)
open! Dune_engine

open Import

val rules :
     sctx:Super_context.t
  -> expander:Expander.t
  -> dir:Path.Build.t
  -> (Cram_test.t, File_tree.Dir.error) result list
  -> unit
