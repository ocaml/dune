open! Import

module Expanded : sig
  type t

  val to_dyn : t -> Dyn.t
  val matches : t -> string list

  (** The component of the glob before the final "/". This is guaranteed to be
      a common prefix of all matches patches. *)
  val prefix : t -> string
end

(** There are different contexts within which globs can be expanded, and this
    signature generalizes the [expand] function over them. These contexts affect
    the expressive power available in [f] when expanding [String_with_vars.t]s
    (e.g. the [Action_builder] context allows evaluating rules during expansion
    while the [Memo] context does not). *)

(** Expand a glob to a memoized list of strings corresponding to paths that
    matched the glob. *)
val memo
  :  Dep_conf.Glob_files.t
  -> f:(String_with_vars.t -> Value.t Memo.t)
  -> base_dir:Path.Build.t
  -> Expanded.t Memo.t

(** Expand a glob inside the [Action_builder] context. The result of calling
    [Glob_files.Action_builder.expand] is an action builder which will resolve
    to the list of strings containing paths matching the glob, and whose
    dependencies will include the file selector built from the glob. *)
val action_builder
  :  Dep_conf.Glob_files.t
  -> f:(String_with_vars.t -> Value.t Action_builder.t)
  -> base_dir:Path.Build.t
  -> Expanded.t Action_builder.t
