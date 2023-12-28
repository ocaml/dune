open Import

(** Define a value that is computed from the env stanza. This function is not
    memoized, so it's best used when the computation is simple. *)
val value
  :  default:'a
  -> dir:Path.Build.t
  -> f:(Dune_env.config -> 'a option Memo.t)
  -> 'a Memo.t

val value_opt
  :  dir:Path.Build.t
  -> f:(Dune_env.config -> 'a option Memo.t)
  -> 'a option Memo.t

val profile : dir:Path.Build.t -> Profile.t Memo.t
val bin_annot : dir:Path.Build.t -> bool Memo.t
val inline_tests : dir:Path.Build.t -> Dune_env.Inline_tests.t Memo.t

(** [inherited ~name ~root ~f] create a function that computes a value derived
    from the env stanza for every directory.

    [root] is used to set the top level value for every project.

    [f] is used to compute the value for every single env stanza definition. *)
val inherited
  :  name:string
  -> root:(Context_name.t -> Dune_project.t -> 'a Memo.t)
  -> f:(parent:'a Memo.t -> dir:Path.Build.t -> Dune_env.config -> 'a Memo.t)
  -> (Path.Build.t -> 'a Memo.t) Staged.t
