(** The core of the build system *)

open Import

(** Build a target, which may be a file or a directory. *)
val build_file : Path.t -> unit Memo.t

(** Build a directory. *)
val build_dir : Path.t -> unit Memo.t

(** Build a file and read its contents with [f]. The execution of [f] is not memoized, so
    call sites should be careful to avoid duplicating [f]'s work. *)
val with_file : Path.t -> f:(Path.t -> 'a) -> 'a Memo.t

(** Build a file and read its contents. Like [with_file ~f:Io.read_file] but memoized. *)
val read_file : Path.t -> string Memo.t

(** Return [true] if a file or directory exists or is buildable. *)
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
val execute_action_stdout : Rule.Anonymous_action.t Action_builder.t -> string Memo.t

type rule_execution_result =
  { facts : Dep.Fact.t Dep.Map.t
  ; targets : Digest.t Targets.Produced.t
  }

val execute_rule : Rule.t -> rule_execution_result Memo.t
val dep_on_alias_definition : Rules.Dir_rules.Alias_spec.item -> unit Action_builder.t

(** {2 Sharing the rule execution pipeline}

    These functions are part of the ordinary rule execution pipeline and are
    exposed so that [dune shell] can prepare a rule action without executing
    it. *)

type prepared_rule_action =
  { sandbox : Sandbox.t
  ; action : Action.t
  ; root : Path.t
  ; context : Build_context.t option
  ; env : Env.t
  ; targets : Targets.Validated.t
  ; rule_loc : Loc.t
  ; execution_parameters : Execution_parameters.t
  }

(** Evaluate the rule's action and build the facts of its dependencies. *)
val evaluate_rule_action
  :  Rule.t
  -> (Action.Full.t * Dep.Facts.t * Execution_parameters.t) Memo.t

(** Remove the rule's declared targets both from the digest table and from
    the build directory. *)
val remove_rule_targets : Targets.Validated.t -> unit Fiber.t

(** Prepare the rule's action in its normally selected sandbox and run [f]
    while the sandbox and the rule's action locks are held. *)
val with_prepared_action_for_rule
  :  rule_digest:Digest.t
  -> action:Action.Full.t
  -> facts:Dep.Facts.t
  -> loc:Loc.t
  -> execution_parameters:Execution_parameters.t
  -> sandbox_mode:Sandbox_mode.some option
  -> targets:Targets.Validated.t
  -> f:(prepared_rule_action -> 'a Fiber.t)
  -> 'a Fiber.t

(** Select the sandbox mode for a rule given its sandboxing configuration and
    the sandboxing preference. *)
val select_sandbox_mode
  :  Sandbox_config.t
  -> loc:Loc.t
  -> sandboxing_preference:Sandbox_mode.t list
  -> Sandbox_mode.t

(** Compute the digest of a rule. This determines the shared-cache key and the
    name of the rule's sandbox directory. *)
val compute_rule_digest
  :  Rule.t
  -> facts:Dep.Facts.t
  -> action:Action.Full.t
  -> sandbox_mode:Sandbox_mode.t
  -> execution_parameters:Execution_parameters.t
  -> Digest.t

(** {2 Running the build system} *)

val run
  :  ?restart_started_at:Time.t
  -> ?build:Process.Build.t
  -> (unit -> 'a Memo.t)
  -> ('a, [ `Already_reported ]) Result.t Fiber.t

(** A variant of [run] that raises an [Already_reported] exception on error. *)
val run_exn : (unit -> 'a Memo.t) -> 'a Fiber.t

module Request : sig
  module Goal : sig
    type t

    val create : unit Action_builder.t -> t
    val await : t -> Build_outcome.t Fiber.t
    val is_finished : t -> bool
    val complete : t -> Build_outcome.t -> unit Fiber.t
  end

  type t

  val create : Goal.t list -> t

  (** Prevent this request from completing goals that have not already
      completed. *)
  val cancel_completion : t -> unit
end

val run_build_requests
  :  ?restart_started_at:Time.t
  -> build_started_at:Time.t
  -> ?build:Process.Build.t
  -> Request.t
  -> (unit, [ `Already_reported ]) result Fiber.t

(** {2 Misc} *)

module Progress : sig
  (** Measures for the progress of the build. *)

  type t =
    { number_of_rules_discovered : int
    ; number_of_rules_validated : int
    ; number_of_rules_failed : int
    }

  (** Initialize with zeros on all measures. *)
  val init : t

  (** Rules that have become live but are not yet validated this run
      ([number_of_rules_discovered - number_of_rules_validated]). *)
  val number_of_rules_in_progress : t -> int
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

val state : State.t ref

(** The current set of active errors. *)
val errors : Build_system_error.Set.t Fiber.Svar.t
