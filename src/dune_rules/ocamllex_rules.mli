open Import

val gen_rules
  :  sctx:Super_context.t
  -> dir_contents:Dir_contents.t
  -> dir:Path.Build.t
  -> Ocamllex.t
  -> unit Memo.t
