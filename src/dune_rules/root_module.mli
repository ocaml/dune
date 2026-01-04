open Import

val entry_module_names : Super_context.t -> Lib.t -> Module_name.t list Resolve.Memo.t

val entries
  :  Super_context.t
  -> requires_compile:Lib.t list Resolve.Memo.t
  -> Module_name.t list Action_builder.t
