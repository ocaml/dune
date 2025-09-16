open Import

val foreign_flags
  :  dir:Path.Build.t
  -> expander:Expander.t
  -> flags:Ordered_set_lang.Unexpanded.t
  -> language:Foreign_language.t
  -> string list Action_builder.t

val build_o_files
  :  sctx:Super_context.t
  -> foreign_sources:Foreign.Sources.t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> requires:Lib.t list Resolve.t
  -> dir_contents:Dir_contents.t
  -> Path.t Mode.Map.Multi.t Memo.t

val foreign_flags_env
  :  dir:Path.Build.t
  -> string list Action_builder.t Foreign_language.Dict.t Memo.t
