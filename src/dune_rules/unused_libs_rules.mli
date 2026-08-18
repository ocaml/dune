open Import

val gen_rules_for_context
  :  Compilation_context.t
  -> Lib.Compile.t
  -> loc:Loc.t
  -> unit Memo.t
