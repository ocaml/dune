open Import

val value
  :  default:'a
  -> dir:Path.Build.t
  -> f:(Dune_env.config -> 'a option Memo.t)
  -> 'a Memo.t

val bin_annot : dir:Path.Build.t -> bool Memo.t
val inline_tests : dir:Path.Build.t -> Dune_env.Inline_tests.t Memo.t
