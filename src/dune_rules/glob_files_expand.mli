open! Import

(** There are different contexts within which globs can be expanded, and this
    signature generalizes the [expand] function over them. These contexts affect
    the expressive power available in [f] when expanding [String_with_vars.t]s
    (e.g. the [Action_builder] context allows evaluating rules during expansion
    while the [Memo] context does not). *)

(** Expand a glob to a memoized list of strings corresponding to paths that
    matched the glob. *)
val memo :
     Dep_conf.Glob_files.t
  -> f:(String_with_vars.t -> string Memo.t)
  -> base_dir:Path.Build.t
  -> string list Memo.t

(** Expand a glob inside the [Action_builder] context. The result of calling
    [Glob_files.Action_builder.expand] is an action builder which will resolve
    to the list of strings containing paths matching the glob, and whose
    dependencies will include the file selector built from the glob. *)
val action_builder :
     Dep_conf.Glob_files.t
  -> f:(String_with_vars.t -> string Action_builder.t)
  -> base_dir:Path.Build.t
  -> string list Action_builder.t
