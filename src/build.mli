(** The build arrow *)

open! Stdune
open! Import

type ('a, 'b) t

val arr : ('a -> 'b) -> ('a, 'b) t

(* In the post-arrow Dune, we will have the selective functor [Build] instead of
the arrow [Build]. To achieve a gradual transition, we introduce a type synonym
which makes a unit arrow look like a functor. *)
type 'a s = (unit, 'a) t

val return : 'a -> 'a s

module O : sig
  val ( >>> ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  val ( ^>> ) : ('a -> 'b) -> ('b, 'c) t -> ('a, 'c) t
  val ( >>^ ) : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t
  val ( *** ) : ('a, 'b) t -> ('c, 'd) t -> ('a * 'c, 'b * 'd) t
  val ( &&& ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
end

val first  : ('a, 'b) t -> ('a * 'c, 'b * 'c) t
val second : ('a, 'b) t -> ('c * 'a, 'c * 'b) t

(** Same as [O.(&&&)]. Sends the input to both argument arrows and combine their output.

    The default definition may be overridden with a more efficient version if desired. *)
val fanout  : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
val fanout3 : ('a, 'b) t -> ('a, 'c) t -> ('a, 'd) t ->  ('a, 'b * 'c * 'd) t
val fanout4 : ('a, 'b) t -> ('a, 'c) t -> ('a, 'd) t -> ('a, 'e) t -> ('a, 'b * 'c * 'd * 'e) t

val all : ('a, 'b) t list -> ('a, 'b list) t

(** Optimization to avoiding eagerly computing a [Build.t] value,
    assume it contains no targets. *)
val lazy_no_targets : ('a, 'b) t Lazy.t -> ('a, 'b) t

(* CR-someday diml: this API is not great, what about:

   {[
     module Action_with_deps : sig
       type t
       val add_file_dependency : t -> Path.t -> t
     end

     (** Same as [t >>> arr (fun x -> Action_with_deps.add_file_dependency x p)]
         but better as [p] is statically known *)
     val record_dependency
       :  Path.t
       -> ('a, Action_with_deps.t) t
       -> ('a, Action_with_deps.t) t
   ]}
*)
(** [path p] records [p] as a file that is read by the action produced by the
    build arrow. *)
val path  : Path.t -> ('a, 'a) t

val dep : Dep.t -> ('a, 'a) t
val deps : Dep.Set.t -> ('a, 'a) t

val paths : Path.t list -> ('a, 'a) t
val path_set : Path.Set.t -> ('a, 'a) t

(** Evaluate a predicate against all targets and record all the matched files as
    dependencies of the action produced by the build arrow. *)
val paths_matching : loc:Loc.t -> File_selector.t -> ('a, Path.Set.t) t

(* TODO: We always ignore the resulting [bool] -- shall we return [unit]? *)
(** [paths_existing paths] will require as dependencies the files that
    actually exist, and return true if the all the paths do actually exist. *)
val paths_existing : Path.t list -> ('a, bool) t

(** [env_var v] records [v] as an environment variable that is read by the
    action produced by the build arrow. *)
val env_var : string -> ('a, 'a) t

val alias : Alias.t -> ('a, 'a) t

(** Record a set of targets of the action produced by the build arrow. *)
val declare_targets : Path.Build.Set.t -> ('a, 'a) t

(** Compute the set of source of all files present in the sub-tree
    starting at [dir] and record them as dependencies. *)
val source_tree
  :  dir:Path.t
  -> file_tree:File_tree.t
  -> ('a, Path.Set.t) t

(** Record dynamic dependencies *)
val dyn_paths : ('a, Path.t list) t -> ('a, 'a) t
val dyn_path_set : ('a, Path.Set.t) t -> ('a, 'a) t

(** [catch t ~on_error] evaluates to [on_error exn] if exception [exn] is
    raised during the evaluation of [t]. *)
val catch : ('a, 'b) t -> on_error:(exn -> 'b) -> ('a, 'b) t

(** [contents path] returns an arrow that when run will return the contents of
    the file at [path]. *)
val contents : Path.t -> ('a, string) t

(** [lines_of path] returns an arrow that when run will return the contents of
    the file at [path] as a list of lines. *)
val lines_of : Path.t -> ('a, string list) t

(** [strings path] is like [lines_of path] except each line is unescaped using
    the OCaml conventions. *)
val strings : Path.t -> ('a, string list) t

(** Load an S-expression from a file *)
val read_sexp : Path.t -> Dune_lang.File_syntax.t -> (unit, Dune_lang.Ast.t) t

(** Evaluates to [true] if the file is present on the file system or is the target of a
    rule. *)
val file_exists : Path.t -> ('a, bool)  t

(** [if_file_exists p ~then ~else] is an arrow that behaves like [then_] if [file_exists
    p] evaluates to [true], and [else_] otherwise. *)
val if_file_exists : Path.t -> then_:('a, 'b) t -> else_:('a, 'b) t -> ('a, 'b) t

(** [file_exists_opt p t] is:

    {[
      if_file_exists p ~then_:(t >>^ fun x -> Some x) ~else_:(arr (fun _ -> None))
    ]}
*)
val file_exists_opt : Path.t -> ('a, 'b) t -> ('a, 'b option) t

(** Always fail when executed. We pass a function rather than an
    exception to get a proper backtrace *)
val fail : ?targets:Path.Build.t list -> fail -> (_, _) t

val of_result
  :  ?targets:Path.Build.t list
  -> ('a, 'b) t Or_exn.t
  -> ('a, 'b) t

val of_result_map
  : ?targets:Path.Build.t list
  -> 'a Or_exn.t
  -> f:('a -> ('b, 'c) t)
  -> ('b, 'c) t

(** [memoize name t] is an arrow that behaves like [t] except that its
    result is computed only once. *)
val memoize : string -> (unit, 'a) t -> (unit, 'a) t

val action
  :  ?dir:Path.t
  -> targets:Path.Build.t list
  -> Action.t
  -> (_, Action.t) t

val action_dyn
  :  ?dir:Path.t
  -> targets:Path.Build.t list
  -> unit
  -> (Action.t, Action.t) t

(** Create a file with the given contents. *)
val write_file : Path.Build.t -> string -> (unit, Action.t) t
val write_file_dyn : Path.Build.t -> (string, Action.t) t

val copy : src:Path.t -> dst:Path.Build.t -> (unit, Action.t) t
val copy_and_add_line_directive
  :  src:Path.t
  -> dst:Path.Build.t
  -> (unit, Action.t) t

val symlink : src:Path.t -> dst:Path.Build.t -> (unit, Action.t) t

val create_file : Path.Build.t -> (_, Action.t) t
val remove_tree : Path.Build.t -> (_, Action.t) t
val mkdir : Path.Build.t -> (_, Action.t) t

(** Merge a list of actions *)
val progn : ('a, Action.t) t list -> ('a, Action.t) t

val record_lib_deps : Lib_deps_info.t -> ('a, 'a) t

(** {1 Analysis} *)

(** Must be called first before [lib_deps] and [targets] as it updates
    some of the internal references in the build arrow. *)
val static_deps
  :  (_, _) t
  -> all_targets:(dir:Path.t -> Path.Set.t)
  -> Static_deps.t

val lib_deps
  :  (_, _) t
  -> Lib_deps_info.t

val targets
  :  (_, _) t
  -> Path.Build.Set.t

(** {1 Execution} *)

(** Executes a build arrow. Returns the result and the set of dynamic
    dependencies discovered during execution. *)
val exec : eval_pred:Dep.eval_pred -> ('a, 'b) t -> 'a -> 'b * Dep.Set.t

(**/**)
val paths_for_rule : Path.Set.t -> ('a, 'a) t

val merge_files_dyn
  :  target:Path.Build.t
  -> (Path.t list * string list, Action.t) t

val ignore : ('a, 'b) t -> ('a, unit) t

(* A module with standard combinators for applicative and selective functors, as
well as equivalents of the functions from the arrow-based API. *)
module S : sig
  module O : sig
    val (let+)  : 'a s -> ('a -> 'b) -> 'b s
    val (and+)  : 'a s -> 'b s -> ('a * 'b) s
    val ( >>> ) : unit s -> 'a s -> 'a s
    val ( >>^ ) : 'a s -> ('a -> 'b) -> 'b s
    val ( &&& ) : 'a s -> 'b s -> ('a * 'b) s
  end

  val apply  : 'a s        -> ('a -> 'b) s -> 'b s
  val map    : 'a s        -> f:('a -> 'b) -> 'b s
  val seq    : unit s      -> 'a s         -> 'a s
  val seqs   : unit s list -> 'a s         -> 'a s
  val ignore : 'a s        -> unit s

  (* In future this will likely replace the [Dyn_deps] constructor. *)
  val dyn_deps : ('a * Dep.Set.t) s -> 'a s
end
