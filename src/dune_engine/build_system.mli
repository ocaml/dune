(** The core of the build system *)

open Import

(** Build a file. *)
val build_file : Path.t -> unit Memo.t

(** Build a file and read its contents with [f]. The execution of [f] is not memoized, so
    call sites should be careful to avoid duplicating [f]'s work. *)
val with_file : Path.t -> f:(Path.t -> 'a) -> 'a Memo.t

(** Build a file and read its contents. Like [with_file ~f:Io.read_file] but memoized. *)
val read_file : Path.t -> string Memo.t

(** Return [true] if a file exists or is buildable *)
val file_exists : Path.t -> bool Memo.t

(** Build a set of dependencies and return learned facts about them. *)
val build_deps : Dep.Set.t -> Dep.Facts.t Memo.t

(** Record the given set as dependencies of the action produced by the action builder. *)
val record_deps : Dep.Set.t -> unit Action_builder.t

(** [eval_pred glob] returns the set of filenames in [File_selector.dir glob] that matches
    [File_selector.predicate glob], including both sources and generated files.

    This function does the minimum amount of work necessary to produce the result, and may
    do some building (e.g., if [glob] points inside a directory target). To force building
    the files you need, use [build_file]. *)
val eval_pred : File_selector.t -> Filename_set.t Memo.t

(** Same as [eval_pred] with [Predicate.true_] as predicate. *)
val files_of : dir:Path.t -> Filename_set.t Memo.t

(** Execute an action. The execution is cached. *)
val execute_action : observing_facts:Dep.Facts.t -> Rule.Anonymous_action.t -> unit Memo.t

(** Execute an action and capture its stdout. The execution is cached. *)
val execute_action_stdout
  :  observing_facts:Dep.Facts.t
  -> Rule.Anonymous_action.t
  -> string Memo.t

type rule_execution_result =
  { facts : Dep.Fact.t Dep.Map.t
  ; targets : Digest.t Targets.Produced.t
  }

val execute_rule : Rule.t -> rule_execution_result Memo.t
val dep_on_alias_definition : Rules.Dir_rules.Alias_spec.item -> unit Action_builder.t

(** {2 Running the build system} *)

val run : (unit -> 'a Memo.t) -> ('a, [ `Already_reported ]) Result.t Fiber.t

(** A variant of [run] that raises an [Already_reported] exception on error. *)
val run_exn : (unit -> 'a Memo.t) -> 'a Fiber.t

(** {2 Misc} *)

module Progress : sig
  (** Measures for the progress of the build. *)

  type t =
    { number_of_rules_discovered : int
    ; number_of_rules_executed : int
    ; number_of_rules_failed : int
    }

  (** Initialize with zeros on all measures. *)
  val init : t
end

module State : sig
  type t =
    | Initializing
    | Building of Progress.t
    | Restarting_current_build
    | Build_succeeded__now_waiting_for_changes
    | Build_failed__now_waiting_for_changes

  val equal : t -> t -> bool
end

val state : State.t Fiber.Svar.t

(** The current set of active errors. *)
val errors : Build_system_error.Set.t Fiber.Svar.t
