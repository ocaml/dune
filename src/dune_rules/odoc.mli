(** Odoc rules *)

open Import

val setup_library_odoc_rules :
  Compilation_context.t -> Lib.Local.t -> unit Memo.t

val gen_project_rules : Super_context.t -> Dune_project.t -> unit Memo.t

val setup_private_library_doc_alias :
     Super_context.t
  -> scope:Scope.t
  -> dir:Stdune.Path.Build.t
  -> Dune_file.Library.t
  -> unit Memo.t

val gen_rules :
     Super_context.t
  -> dir:Path.Build.t
  -> string list
  -> Build_config.gen_rules_result Memo.t
