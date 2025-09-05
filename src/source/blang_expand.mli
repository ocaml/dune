open Import

val eval
  :  Blang.t
  -> dir:Path.t
  -> f:Value.t list Memo.t String_with_vars.expander
  -> bool Memo.t
