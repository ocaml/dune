(** Workspace-local and shared caches for rules. *)

open! Stdune
open! Import

(** A type isomorphic to [Result], but without the negative connotations
    associated with the word "error". *)
module Result : sig
  type ('hit, 'miss) t =
    | Hit of 'hit
    | Miss of 'miss
end

module Workspace_local : sig
  module Miss_reason : sig
    type t =
      | No_previous_record
      | Rule_changed of Digest.t * Digest.t
      | Targets_changed
      | Targets_missing
      | Dynamic_deps_changed
      | Always_rerun
  end

  (** Check if the workspace-local cache contains up-to-date results for a rule
      using the information stored in the rule database. *)
  val lookup :
       always_rerun:bool
    -> rule_digest:Digest.t
    -> targets:Targets.Validated.t
    -> env:Env.t
    -> build_deps:(Dep.Set.t -> Dep.Facts.t Memo.Build.t)
    -> (Digest.t Targets.Produced.t, Miss_reason.t) Result.t Fiber.t

  val report_miss : head_target:Path.Build.t -> Miss_reason.t -> unit

  (** Add a new record to the rule database. *)
  val store :
       head_target:Path.Build.t
    -> rule_digest:Digest.t
    -> dynamic_deps_stages:(Action_exec.Dynamic_dep.Set.t * Digest.t) list
    -> targets_digest:Digest.t
    -> unit
end

module Shared_cache : sig
  module Miss_reason : sig
    type t =
      | Cache_disabled
      | Cannot_go_in_shared_cache
      | Rerunning_for_reproducibility_check
      | Not_found_in_cache
      | Error of string
  end

  (** Check if the shared cache contains results for a rule and decide whether
      to use these results or rerun the rule for a reproducibility check. *)
  val lookup :
       can_go_in_shared_cache:bool
    -> cache_config:Dune_cache.Config.t
    -> debug_shared_cache:bool
    -> rule_digest:Digest.t
    -> targets:Targets.Validated.t
    -> target_dir:Path.Build.t
    -> (Digest.t Targets.Produced.t, Miss_reason.t) Result.t

  val report_miss :
    rule_digest:Digest.t -> head_target:Path.Build.t -> Miss_reason.t -> unit

  (** This function performs the following steps:

      - Check that action produced all expected targets;

      - Compute their digests;

      - Remove write permissions from the targets;

      - Store results to the shared cache if needed. *)
  val examine_targets_and_store :
       cache_config:Dune_cache.Config.t
    -> can_go_in_shared_cache:bool
    -> loc:Loc.t
    -> rule_digest:Digest.t
    -> execution_parameters:Execution_parameters.t
    -> action:Action.t
    -> produced_targets:unit Targets.Produced.t
    -> Digest.t Targets.Produced.t Fiber.t
end
