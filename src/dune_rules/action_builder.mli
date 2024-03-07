open Import

include
  module type of Dune_engine.Action_builder
  with type 'a t = 'a Dune_engine.Action_builder.t

(** Like [of_memo] but collapses the two levels of [t]. *)
val of_memo_join : 'a t Memo.t -> 'a t

(** Delay a static computation until the description is evaluated *)
val delayed : (unit -> 'a) -> 'a t

val ignore : 'a t -> unit t

type fail = { fail : 'a. unit -> 'a }

(** Always fail when executed. We pass a function rather than an exception to
    get a proper backtrace *)
val fail : fail -> _ t

module With_targets : module type of With_targets with type 'a t = 'a With_targets.t

(** [path p] records [p] as a file that is read by the action produced by the
    action builder. *)
val path : Path.t -> unit t

val dep : Dep.t -> unit t
val deps : Dep.Set.t -> unit t
val dyn_deps : ('a * Dep.Set.t) t -> 'a t
val paths : Path.t list -> unit t
val path_set : Path.Set.t -> unit t

(** [dyn_memo_deps m] adds the dependencies computed by [m] while returning the
    extra value. *)
val dyn_memo_deps : (Dep.Set.t * 'a) Memo.t -> 'a t

(** Record dynamic dependencies *)
val dyn_paths : ('a * Path.t list) t -> 'a t

val dyn_paths_unit : Path.t list t -> unit t

(** [lines_of path] returns a description that when run will return the contents
    of the file at [path] as a list of lines. *)
val lines_of : Path.t -> string list t

(** Load an S-expression from a file *)
val read_sexp : Path.t -> Dune_sexp.Ast.t t

val symlink_dir : src:Path.t -> dst:Path.Build.t -> Action.Full.t With_targets.t
val symlink : src:Path.t -> dst:Path.Build.t -> Action.Full.t With_targets.t
val copy : src:Path.t -> dst:Path.Build.t -> Action.Full.t With_targets.t

(** Merge a list of actions accumulating the sets of their targets. *)
val progn : Action.Full.t With_targets.t list -> Action.Full.t With_targets.t

(** Create a file with the given contents. *)
val write_file
  :  ?perm:Action.File_perm.t
  -> Path.Build.t
  -> string
  -> Action.Full.t With_targets.t

val write_file_dyn
  :  ?perm:Action.File_perm.t
  -> Path.Build.t
  -> string t
  -> Action.Full.t With_targets.t

val with_stdout_to
  :  ?perm:Action.File_perm.t
  -> Path.Build.t
  -> Action.Full.t t
  -> Action.Full.t With_targets.t

(** [contents path] returns a description that when run will return the contents
    of the file at [path]. *)
val contents : Path.t -> string t

(** Evaluates to [true] if the file is present on the file system or is the
    target of a rule. It doesn't add the path as dependency *)
val file_exists : Path.t -> bool t

(** [if_file_exists p ~then ~else] is a description that behaves like [then_] if
    [file_exists p] evaluates to [true], and [else_] otherwise. *)
val if_file_exists : Path.t -> then_:'a t -> else_:'a t -> 'a t

(** [paths_existing paths] will require as dependencies the files that actually
    exist. *)
val paths_existing : Path.t list -> unit t

(** Evaluate a predicate against all targets and record all the matched files as
    dependencies of the action produced by the action builder. *)
val paths_matching : loc:Loc.t -> File_selector.t -> Filename_set.t t

(** Like [paths_matching], but don't return the resulting set. The action
    dependency is still registered. *)
val paths_matching_unit : loc:Loc.t -> File_selector.t -> unit t

(** [env_var v] records [v] as an environment variable that is read by the
    action produced by the action builder. *)
val env_var : string -> unit t

(** Add targets to an action builder, turning a target-less [Action_builder.t]
    into [With_targets.t]. *)
val with_targets : 'a t -> targets:Targets.t -> 'a With_targets.t

(** Like [with_targets] but specifies a list of file targets. *)
val with_file_targets : 'a t -> file_targets:Path.Build.t list -> 'a With_targets.t

(** Create a value of [With_targets.t] with the empty set of targets. *)
val with_no_targets : 'a t -> 'a With_targets.t
