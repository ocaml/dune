open Import

val setup_of_ocaml_rules :
     sctx:Super_context.t
  -> dir:Path.Build.t
  -> Coq_of_ocaml_stanza.t
  -> unit Memo.t
