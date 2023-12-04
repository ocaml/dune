open Import

val gen_project_rules : Super_context.t -> Dune_project.t -> unit Memo.t

val gen_rules
  :  Super_context.t
  -> dir:Path.Build.t
  -> string list
  -> Build_config.Gen_rules.t Memo.t
