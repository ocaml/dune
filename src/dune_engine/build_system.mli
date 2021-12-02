(** The core of the build system *)

open! Stdune
open! Import
module Action_builder := Action_builder0

(** {1 Requests} *)

(** Build a file and return the digest of its contents. *)
val build_file : Path.t -> Digest.t Memo.Build.t

(** Build a file and access its contents with [f]. *)
val read_file : Path.t -> f:(Path.t -> 'a) -> 'a Memo.Build.t

(** Build a set of dependencies and return learned facts about them. *)
val build_deps : Dep.Set.t -> Dep.Facts.t Memo.Build.t

(** [eval_pred glob] returns the list of files in [File_selector.dir glob] that
    matches [File_selector.predicate glob]. The list of files includes the list
    of file targets. Currently, this function ignores directory targets, which
    is a limitation we'd like to remove in future. *)
val eval_pred : File_selector.t -> Path.Set.t Memo.Build.t

(** Like [eval_pred] but also builds the resulting set of files. This function
    doesn't have [eval_pred]'s limitation about directory targets and takes them
    into account. *)
val build_pred : File_selector.t -> Dep.Fact.Files.t Memo.Build.t

(** Assuming [files] is a set of files in [_build/install] that belong to a
    package [pkg], [package_deps packages_of pkg files] is the set of direct
    package dependencies of [package]. *)
val package_deps :
     packages_of:(Path.Build.t -> Package.Id.Set.t Memo.Build.t)
  -> Package.t
  -> Path.Build.t list
  -> Package.Id.Set.t Memo.Build.t

(** Execute an action. The execution is cached. *)
val execute_action :
  observing_facts:Dep.Facts.t -> Rule.Anonymous_action.t -> unit Memo.Build.t

(** Execute an action and capture its stdout. The execution is cached. *)
val execute_action_stdout :
  observing_facts:Dep.Facts.t -> Rule.Anonymous_action.t -> string Memo.Build.t

val dep_on_alias_definition :
  Rules.Dir_rules.Alias_spec.item -> unit Action_builder.t

(** {2 Running the build system} *)

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

val get_current_progress : unit -> Progress.t

(** The current set of active errors. *)
val errors : unit -> Build_config.Error.t list

(** Returns the last event reported to the handler. *)
val last_event : unit -> Build_config.Handler.event option
