(** Odoc rules *)

open Import

module Paths : sig
  val toplevel_index : Context.t -> Path.Build.t
end

val find_project_by_key : Dune_project.File_key.t -> Dune_project.t Memo.t

module Scope_key : sig
  val of_string : Context_name.t -> string -> (Lib_name.t * Lib.DB.t) Memo.t
  val to_string : Lib_name.t -> Dune_project.t -> string
end

val lib_unique_name : Lib.t -> string
val odoc_program : Super_context.t -> Path.Build.t -> Action.Prog.t Action_builder.t

val check_mlds_no_dupes
  :  pkg:Dune_lang.Package_name.t
  -> mlds:Path.Build.t list
  -> Path.Build.t Import.String.Map.t

val libs_of_pkg : Context_name.t -> pkg:Package.Name.t -> Lib.Local.t list Memo.t

val entry_modules
  :  Super_context.t
  -> pkg:Dune_lang.Package_name.t
  -> Module.t list Lib.Local.Map.t Memo.t

val run_odoc
  :  Super_context.t
  -> dir:Path.t
  -> string
  -> quiet:bool
  -> flags_for:Path.Build.t option
  -> Command.Args.any Command.Args.t list
  -> Action.Full.t Action_builder.With_targets.t

val setup_library_odoc_rules : Compilation_context.t -> Lib.Local.t -> unit Memo.t
val gen_project_rules : Super_context.t -> Dune_project.t -> unit Memo.t

val setup_private_library_doc_alias
  :  Super_context.t
  -> scope:Scope.t
  -> dir:Stdune.Path.Build.t
  -> Library.t
  -> unit Memo.t

val gen_rules
  :  Super_context.t
  -> dir:Path.Build.t
  -> string list
  -> Build_config.Gen_rules.result Memo.t
