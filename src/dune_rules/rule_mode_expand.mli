open Import

val expand_path
  :  Rule_mode.t
  -> expander:Expander.t
  -> dir:Path.Build.t
  -> Rule.Mode.t Memo.t

val expand_str : Rule_mode.t -> expander:Expander.t -> Rule.Mode.t Memo.t
