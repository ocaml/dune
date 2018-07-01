(** Dune files that are installed on the system *)

open Stdune

val load : Path.t -> Jbuild.Sub_system_info.t Sub_system_name.Map.t
val gen
  : lang:File_tree.Dune_file.Kind.t
  -> (Syntax.Version.t * Sexp.t) Sub_system_name.Map.t
  -> Sexp.t
