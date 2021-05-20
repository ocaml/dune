(** Action builder *)

open! Stdune
open! Import

type 'a t

module O : sig
  val ( >>> ) : unit t -> 'a t -> 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

module With_targets : sig
  type 'a build

  type nonrec 'a t =
    { build : 'a t
    ; targets : Path.Build.Set.t
    }

  val map_build : 'a t -> f:('a build -> 'b build) -> 'b t

  val return : 'a -> 'a t

  val add : 'a t -> targets:Path.Build.t list -> 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

  val write_file_dyn :
    ?perm:Action.File_perm.t -> Path.Build.t -> string t -> Action.t t

  val all : 'a t list -> 'a list t

  (** [memoize name t] is an action builder that behaves like [t] except that
      its result is computed only once. *)
  val memoize : string -> 'a t -> 'a t

  module O : sig
    val ( >>> ) : unit t -> 'a t -> 'a t

    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  end
end
with type 'a build := 'a t

(** Add a set of targets to an action builder, turning a target-less
    [Action_builder.t] into [Action_builder.With_targets.t]. *)
val with_targets : 'a t -> targets:Path.Build.t list -> 'a With_targets.t

(** [with_targets_set] is like [with_targets] but [targets] is a set *)
val with_targets_set : 'a t -> targets:Path.Build.Set.t -> 'a With_targets.t

(** Create a value of [With_targets.t] with the empty set of targets. *)
val with_no_targets : 'a t -> 'a With_targets.t

val return : 'a -> 'a t

val bind : 'a t -> f:('a -> 'b t) -> 'b t

val map : 'a t -> f:('a -> 'b) -> 'b t

val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

val both : 'a t -> 'b t -> ('a * 'b) t

val ignore : 'a t -> unit t

val all : 'a t list -> 'a list t

val all_unit : unit t list -> unit t

module List : sig
  val map : 'a list -> f:('a -> 'b t) -> 'b list t
end

(** Delay a static computation until the description is evaluated *)
val delayed : (unit -> 'a) -> 'a t

(** CR-someday diml: this API is not great, what about:

    {[
      module Action_with_deps : sig
        type t
        val add_file_dependency : t -> Path.t -> t
      end

      (** Same as
          [t >>> arr (fun x -> Action_with_deps.add_file_dependency x p)]
          but better as [p] is statically known *)

      val record_dependency
        :  Path.t
        -> ('a, Action_with_deps.t) t
        -> ('a, Action_with_deps.t) t
    ]} *)

(** [path p] records [p] as a file that is read by the action produced by the
    action builder. *)
val path : Path.t -> unit t

val dep : Dep.t -> unit t

val deps : Dep.Set.t -> unit t

val dyn_deps : ('a * Dep.Set.t) t -> 'a t

val paths : Path.t list -> unit t

val path_set : Path.Set.t -> unit t

(** Evaluate a predicate against all targets and record all the matched files as
    dependencies of the action produced by the action builder. *)
val paths_matching : loc:Loc.t -> File_selector.t -> Path.Set.t t

(** Like [paths_matching], but don't return the resulting set. The action
    dependency is still registered. *)
val paths_matching_unit : loc:Loc.t -> File_selector.t -> unit t

(** [paths_existing paths] will require as dependencies the files that actually
    exist. *)
val paths_existing : Path.t list -> unit t

(** [env_var v] records [v] as an environment variable that is read by the
    action produced by the action builder. *)
val env_var : string -> unit t

val alias : Alias.t -> unit t

(** Compute the set of source of all files present in the sub-tree starting at
    [dir] and record them as dependencies. *)
val source_tree : dir:Path.t -> Path.Set.t t

(** Record dynamic dependencies *)
val dyn_paths : ('a * Path.t list) t -> 'a t

val dyn_paths_unit : Path.t list t -> unit t

val dyn_path_set : ('a * Path.Set.t) t -> 'a t

val dyn_path_set_reuse : Path.Set.t t -> Path.Set.t t

(** [contents path] returns a description that when run will return the contents
    of the file at [path]. *)
val contents : Path.t -> string t

(** [lines_of path] returns a description that when run will return the contents
    of the file at [path] as a list of lines. *)
val lines_of : Path.t -> string list t

(** [strings path] is like [lines_of path] except each line is unescaped using
    the OCaml conventions. *)
val strings : Path.t -> string list t

(** Load an S-expression from a file *)
val read_sexp : Path.t -> Dune_lang.Ast.t t

(** Evaluates to [true] if the file is present on the file system or is the
    target of a rule. It doesn't add the path as dependency *)
val file_exists : Path.t -> bool t

(** [if_file_exists p ~then ~else] is a description that behaves like [then_] if
    [file_exists p] evaluates to [true], and [else_] otherwise. *)
val if_file_exists : Path.t -> then_:'a t -> else_:'a t -> 'a t

(** Always fail when executed. We pass a function rather than an exception to
    get a proper backtrace *)
val fail : fail -> _ t

(** [memoize name t] is an action builder that behaves like [t] except that its
    result is computed only once. *)
val memoize : string -> 'a t -> 'a t

(** Create a file with the given contents. *)
val write_file :
  ?perm:Action.File_perm.t -> Path.Build.t -> string -> Action.t With_targets.t

val write_file_dyn :
     ?perm:Action.File_perm.t
  -> Path.Build.t
  -> string t
  -> Action.t With_targets.t

val copy : src:Path.t -> dst:Path.Build.t -> Action.t With_targets.t

val copy_and_add_line_directive :
  src:Path.t -> dst:Path.Build.t -> Action.t With_targets.t

val symlink : src:Path.t -> dst:Path.Build.t -> Action.t With_targets.t

val create_file :
  ?perm:Action.File_perm.t -> Path.Build.t -> Action.t With_targets.t

(** Merge a list of actions accumulating the sets of their targets. *)
val progn : Action.t With_targets.t list -> Action.t With_targets.t

module Action_desc : sig
  (* jeremiedimino: this type correspond to a subset of [Rule.t]. We should
     eventually share the code. *)
  type nonrec t =
    { context : Build_context.t option
    ; action : Action.Full.t
    ; loc : Loc.t option
    ; dir : Path.Build.t
          (** Directory the action is attached to. This is the directory where
              the outcome of the action will be cached. *)
    ; alias : Alias.Name.t option  (** For better error messages *)
    }
end

(** Execute an action. You can think of [action t] as a convenient way of
    declaring an anonymous build rule and depending on its outcome. While the
    return type is [unit], the action might fail. So the outcome here is success
    or failure. This function is commonly used for attaching tests to an alias.

    Note that any dependency declared in [t] is treated as a dependency of the
    action returned by [t], rather than the action currently being computed.
    More precisely, in the following code:

    {[
      let+ () = Action_builder.path p1
      and+ () =
        Action_builder.action
          (let+ () = Action_builder.path p2 in
           act2)
      in
      act1
    ]}

    Dune assumes that:

    - [act1] will read [p1]
    - [act2] will read [p2]

    When passing [--force] to Dune, these are exactly the actions that will be
    re-executed. *)
val action : Action_desc.t t -> unit t

(** Same as [action], but captures the output of the action. *)
val action_stdout : Action_desc.t t -> string t

(** {1 Analysis} *)

(** Returns [Some (x, deps)] if the following can be evaluated statically. *)
val static_eval : 'a t -> ('a * Dep.Set.t) option

(** [goal t] ignores all facts that have been accumulated about the dependencies
    of [t]. For example, [goal (path p)] declares that a path [p] contributes to
    the "goal" of the resulting action builder, which means [p] must be built,
    but the contents of [p] is irrelevant. *)
val goal : 'a t -> 'a t

module Expander : String_with_vars.Expander with type 'a app := 'a t

(** {1 Execution} *)

module Make_exec (Build_deps : sig
  type fact

  val merge_facts : fact Dep.Map.t -> fact Dep.Map.t -> fact Dep.Map.t

  val read_file : Path.t -> f:(Path.t -> 'a) -> 'a Memo.Build.t

  (** Register some newly discover action dependencies *)
  val register_action_deps : Dep.Set.t -> fact Dep.Map.t Memo.Build.t

  (** [register_action_dep_pred fs] is the same as evaluating [fs] and calling
      [register_action_deps (Dep.file_selector fs)], but more efficient as both
      computation require evaluating [fs]. *)
  val register_action_dep_pred :
    File_selector.t -> (Path.Set.t * fact) Memo.Build.t

  (** Check whether a file exists. Returns [true] for files generated by a rule. *)
  val file_exists : Path.t -> bool Memo.Build.t

  (** Check whether an alias is defined. *)
  val alias_exists : Alias.t -> bool Memo.Build.t

  val execute_action :
    observing_facts:fact Dep.Map.t -> Action_desc.t -> unit Memo.Build.t

  val execute_action_stdout :
    observing_facts:fact Dep.Map.t -> Action_desc.t -> string Memo.Build.t
end) : sig
  (** Execute an action builder. Returns the result and the trace of the
      dependencies. *)
  val exec : 'a t -> ('a * Build_deps.fact Dep.Map.t) Memo.Build.t
end

(** If you're thinking of using [Process.run] here, check that: (i) you don't in
    fact need [Command.run], and that (ii) [Process.run] only reads the declared
    build rule dependencies. *)
val memo_build : 'a Memo.Build.t -> 'a t

(** Like [memo_build] but collapses the two levels of [t]. *)
val memo_build_join : 'a t Memo.Build.t -> 'a t

(** If you're thinking of using [Process.run] here, check that: (i) you don't in
    fact need [Command.run], and that (ii) [Process.run] only reads the declared
    build rule dependencies. *)
val dyn_memo_build : 'a Memo.Build.t t -> 'a t

(** A version of [dyn_memo_build] that makes it convenient to declare dynamic
    action dependencies. *)
val dyn_memo_build_deps : ('a * Dep.Set.t) Memo.Build.t t -> 'a t

(**/**)

val dep_on_alias_if_exists : Alias.t -> bool t
