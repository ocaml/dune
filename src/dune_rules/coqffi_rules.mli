open Import

val setup_ffi_rules :
  sctx:Super_context.t -> dir:Path.Build.t -> Coqffi_stanza.t -> unit Memo.t
