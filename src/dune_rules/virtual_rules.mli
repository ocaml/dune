open Import

type t

val setup_copy_rules_for_impl
  :  sctx:Super_context.t
  -> dir:Path.Build.t
  -> t
  -> unit Memo.t

val no_implements : t
val impl : Super_context.t -> lib:Library.t -> scope:Scope.t -> t Memo.t
val impl_modules : t -> Modules.t -> Modules.With_vlib.t
val stubs_o_files : t -> Path.t list
val implements_parameter : t -> Module.t -> Module_name.t option Resolve.Memo.t
val vimpl_exn : t -> Vimpl.t
