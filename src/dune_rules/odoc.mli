(** Odoc rules *)

open Import

module Doc_mode : sig
  type t =
    | Local_only
    | Full
end

module Paths : sig end

val odoc_program : Super_context.t -> Path.Build.t -> Action.Prog.t Action_builder.t
val libs_of_pkg : Context.t -> pkg:Package.Name.t -> Lib.t list Memo.t

val run_odoc
  :  Super_context.t
  -> ?dir:Path.Build.t
  -> string
  -> quiet:bool
  -> flags_for:Path.Build.t option
  -> Command.Args.any Command.Args.t list
  -> Action.Full.t Action_builder.With_targets.t

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
