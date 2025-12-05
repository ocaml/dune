open Import

val gen_rules
  :  sctx:Super_context.t
  -> dir:Path.Build.t
  -> string list
  -> Build_config.Gen_rules.result Memo.t

type instances

val instances
  :  sctx:Super_context.t
  -> db:Lib.db
  -> Lib_dep.t list
  -> instances list Resolve.Memo.t

val print_instances : Buffer.t -> instances list -> unit
