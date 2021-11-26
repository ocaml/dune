(** Build rules *)

open! Stdune
open! Import
module Action_builder := Action_builder0

(** {1 Setup} *)

(** {2 Primitive for rule generations} *)

(** [eval_pred glob] returns the list of files in [File_selector.dir glob] that
    matches [File_selector.predicate glob]. The list of files includes the list
    of targets. *)
val eval_pred : File_selector.t -> Path.Set.t Memo.Build.t

(** Same as [eval_pred] but also build the resulting set of files. *)
val build_pred : File_selector.t -> Dep.Fact.Files.t Memo.Build.t

(** Returns the set of file targets in the given directory. *)
val file_targets_of : dir:Path.t -> Path.Set.t Memo.Build.t

(** Load the rules for this directory. *)
val load_dir : dir:Path.t -> unit Memo.Build.t

(** Assuming [files] is the list of files in [_build/install] that belong to
    package [pkg], [package_deps t pkg files] is the set of direct package
    dependencies of [package]. *)
val package_deps :
     packages_of:(Path.Build.t -> Package.Id.Set.t Memo.Build.t)
  -> Package.t
  -> Path.Set.t
  -> Package.Id.Set.t Memo.Build.t

(** {1 Requests} *)

(** Build a file and return the digest of its contents *)
val build_file : Path.t -> Digest.t Memo.Build.t

(** Build a file and return its contents with [f] *)
val read_file : Path.t -> f:(Path.t -> 'a) -> 'a Memo.Build.t

(** Return [true] if a file exists or is buildable *)
val file_exists : Path.t -> bool Memo.Build.t

val alias_exists : Alias.t -> bool Memo.Build.t

val is_target : Path.t -> bool Memo.Build.t

val build_deps : Dep.Set.t -> Dep.Facts.t Memo.Build.t

(** Execute a action. The execution is cached. *)
val execute_action :
  observing_facts:Dep.Facts.t -> Rule.Anonymous_action.t -> unit Memo.Build.t

(** Execute a action and capture its output. The execution is cached. *)
val execute_action_stdout :
  observing_facts:Dep.Facts.t -> Rule.Anonymous_action.t -> string Memo.Build.t

(** Return the rule that has the given file has target, if any *)
val get_rule : Path.t -> Rule.t option Memo.Build.t

type alias_definition

val dep_on_alias_definition : alias_definition -> unit Action_builder.t

(** Return the definition of an alias *)
val get_alias_definition :
  Alias.t -> (Loc.t * alias_definition) list Memo.Build.t

(** List of all buildable targets. *)
val all_targets : unit -> Path.Build.Set.t Memo.Build.t

(** {2 Running a build} *)

val run :
  (unit -> 'a Memo.Build.t) -> ('a, [ `Already_reported ]) Result.t Fiber.t

(** A variant of [run] that raises an [Already_reported] exception on error. *)
val run_exn : (unit -> 'a Memo.Build.t) -> 'a Fiber.t

(** {2 Misc} *)

module Progress : sig
  type t =
    { number_of_rules_discovered : int
    ; number_of_rules_executed : int
    }

  val complete : t -> int

  val remaining : t -> int

  val is_determined : t -> bool
end

(** The current set of active errors *)
val errors : unit -> Build_config.Error.t list

val get_current_progress : unit -> Progress.t

(** Returns the last event reported to the handler *)
val last_event : unit -> Build_config.Handler.event option
