(** Odoc rules *)

open Import

module Paths : sig
  val toplevel_index : Context.t -> Path.Build.t
end

val lib_unique_name : Lib.t -> string
val odoc_program : Super_context.t -> Path.Build.t -> Action.Prog.t Action_builder.t
val libs_of_pkg : Context_name.t -> pkg:Package.Name.t -> Lib.Local.t list Memo.t

val mlds
  :  Super_context.t
  -> Dune_lang.Package_name.t
  -> ((Path.Build.t * string) list * Doc_sources.mld list) Memo.t

val report_warnings : Doc_sources.mld list -> unit

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
