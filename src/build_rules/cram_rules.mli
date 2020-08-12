(** Rules for setting up cram tests *)
open Import

val rules :
     sctx:Super_context.t
  -> expander:Expander.t
  -> dir:Path.Build.t
  -> (Cram.test, File_tree.Dir.error) result list
  -> unit
