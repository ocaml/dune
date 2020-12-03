(** Rules for setting up cram tests *)
open! Build_api.Api

open Stdune

val rules :
     sctx:Super_context.t
  -> expander:Expander.t
  -> dir:Path.Build.t
  -> (Cram_test.t, File_tree.Dir.error) result list
  -> unit
