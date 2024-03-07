open Import

val setup_copy_rules_for_impl
  :  sctx:Super_context.t
  -> dir:Path.Build.t
  -> Vimpl.t
  -> unit Memo.t

val impl : Super_context.t -> lib:Library.t -> scope:Scope.t -> Vimpl.t option Memo.t
