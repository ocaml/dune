(** The build description *)

open! Stdune
open! Import

type 'a t

val return : 'a -> 'a t

val map : 'a t -> f:('a -> 'b) -> 'b t

val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

val ignore : 'a t -> unit t

module O : sig
  val ( >>> ) : unit t -> 'a t -> 'a t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

val all : 'a t list -> 'a list t

val all_unit : unit t list -> unit t

(** Optimization to avoiding eagerly computing a [Build.t] value, assume it
    contains no targets. *)
val lazy_no_targets : 'a t Lazy.t -> 'a t

(** Delay a static computation until the description is evaluated *)
val delayed : (unit -> 'a) -> 'a t

(* CR-someday diml: this API is not great, what about:

   {[ module Action_with_deps : sig type t val add_file_dependency : t -> Path.t
   -> t end

   (** Same as [t >>> arr (fun x -> Action_with_deps.add_file_dependency x p)]
   but better as [p] is statically known *) val record_dependency : Path.t ->
   ('a, Action_with_deps.t) t -> ('a, Action_with_deps.t) t ]} *)

(** [path p] records [p] as a file that is read by the action produced by the
    build description. *)
val path : Path.t -> unit t

val dep : Dep.t -> unit t

val deps : Dep.Set.t -> unit t

val paths : Path.t list -> unit t

val path_set : Path.Set.t -> unit t

(** Evaluate a predicate against all targets and record all the matched files as
    dependencies of the action produced by the build description. *)
val paths_matching : loc:Loc.t -> File_selector.t -> Path.Set.t t

(** [paths_existing paths] will require as dependencies the files that actually
    exist. *)
val paths_existing : Path.t list -> unit t

(** [env_var v] records [v] as an environment variable that is read by the
    action produced by the build description. *)
val env_var : string -> unit t

val alias : Alias.t -> unit t

(** Record a set of targets of the action produced by the build description. *)
val declare_targets : Path.Build.Set.t -> unit t

(** Compute the set of source of all files present in the sub-tree starting at
    [dir] and record them as dependencies. *)
val source_tree : dir:Path.t -> Path.Set.t t

(** Record dynamic dependencies *)
val dyn_paths : ('a * Path.t list) t -> 'a t

val dyn_paths_unit : Path.t list t -> unit t

val dyn_path_set : ('a * Path.Set.t) t -> 'a t

val dyn_path_set_reuse : Path.Set.t t -> Path.Set.t t

(** [catch t ~on_error] evaluates to [on_error exn] if exception [exn] is raised
    during the evaluation of [t]. *)
val catch : 'a t -> on_error:(exn -> 'a) -> 'a t

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
    target of a rule. *)
val file_exists : Path.t -> bool t

(* CR-soon amokhov: change [Path.t] to [Path.Build.t] in [if_file_exists]. *)

(** [if_file_exists p ~then ~else] is a description that behaves like [then_] if
    [file_exists p] evaluates to [true], and [else_] otherwise. *)
val if_file_exists : Path.t -> then_:'a t -> else_:'a t -> 'a t

(** Always fail when executed. We pass a function rather than an exception to
    get a proper backtrace *)
val fail : ?targets:Path.Build.t list -> fail -> _ t

val of_result : ?targets:Path.Build.t list -> 'a t Or_exn.t -> 'a t

val of_result_map :
  ?targets:Path.Build.t list -> 'a Or_exn.t -> f:('a -> 'b t) -> 'b t

(** [memoize name t] is a build description that behaves like [t] except that
    its result is computed only once. *)
val memoize : string -> 'a t -> 'a t

val action : ?dir:Path.t -> targets:Path.Build.t list -> Action.t -> Action.t t

val action_dyn :
  ?dir:Path.t -> targets:Path.Build.t list -> Action.t t -> Action.t t

(** Create a file with the given contents. *)
val write_file : Path.Build.t -> string -> Action.t t

val write_file_dyn : Path.Build.t -> string t -> Action.t t

val copy : src:Path.t -> dst:Path.Build.t -> Action.t t

val copy_and_add_line_directive : src:Path.t -> dst:Path.Build.t -> Action.t t

val symlink : src:Path.t -> dst:Path.Build.t -> Action.t t

val create_file : Path.Build.t -> Action.t t

val remove_tree : Path.Build.t -> Action.t t

val mkdir : Path.Build.t -> Action.t t

(** Merge a list of actions *)
val progn : Action.t t list -> Action.t t

val record_lib_deps : Lib_deps_info.t -> unit t

(** {1 Analysis} *)

(** Compute static dependencies of a build description, given a function to
    resolve file-existence queries. *)
val static_deps : _ t -> file_exists:(Path.t -> bool) -> Static_deps.t

(** Compute static library dependencies of a build description, given a function
    to resolve file-existence queries. *)
val lib_deps : _ t -> file_exists:(Path.t -> bool) -> Lib_deps_info.t

val targets : _ t -> Path.Build.Set.t

(** {1 Execution} *)

(** Execute a build description, given functions to resolve file-existence
    queries and file selectors. Returns the result and the set of dynamic
    dependencies discovered during execution. *)
val exec :
     'a t
  -> file_exists:(Path.t -> bool)
  -> eval_pred:Dep.eval_pred
  -> 'a * Dep.Set.t

(**/**)

val paths_for_rule : Path.Set.t -> unit t

(* TODO: Document this and a few other non-obvious functions. *)
val merge_files_dyn :
  target:Path.Build.t -> (Path.t list * string list) t -> Action.t t

val dyn_deps : ('a * Dep.Set.t) t -> 'a t
