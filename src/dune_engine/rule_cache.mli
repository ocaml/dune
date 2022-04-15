(** Workspace-local and shared caches for rules. *)

open! Stdune
open! Import

(** The workspace-local cache consists of two components:

    - Build artifacts currently available in the build directory.

    - A database [_build/.db] that maps rule digests to their target digests.

    The database makes it possible to decide if the build directory contains up
    to date results for a given rule. *)
module Workspace_local : sig
  (** Check if the workspace-local cache contains up-to-date results for a rule
      using the information stored in the rule database. *)
  val lookup :
       always_rerun:bool
    -> rule_digest:Digest.t
    -> targets:Targets.Validated.t
    -> env:Env.t
    -> build_deps:(Dep.Set.t -> Dep.Facts.t Memo.t)
    -> Digest.t Targets.Produced.t option Fiber.t

  (** Add a new record to the rule database. *)
  val store :
       head_target:Path.Build.t
    -> rule_digest:Digest.t
    -> dynamic_deps_stages:(Action_exec.Dynamic_dep.Set.t * Digest.t) list
    -> targets_digest:Digest.t
    -> unit
end

(** The shared cache is a separate directory that contains historical build
    artifacts produced in different workspaces. To restore results from the
    shared cache, Dune copes or hardlinks them into the build directory. *)
module Shared : sig
  (** Check if the shared cache contains results for a rule and decide whether
      to use these results or rerun the rule for a reproducibility check. *)
  val lookup :
       can_go_in_shared_cache:bool
    -> rule_digest:Digest.t
    -> targets:Targets.Validated.t
    -> target_dir:Path.Build.t
    -> Digest.t Targets.Produced.t option

  (** This function performs the following steps:

      - Check that action produced all expected targets;

      - Compute their digests;

      - Remove write permissions from the targets;

      - Store results to the shared cache if needed. *)
  val examine_targets_and_store :
       can_go_in_shared_cache:bool
    -> loc:Loc.t
    -> rule_digest:Digest.t
    -> execution_parameters:Execution_parameters.t
    -> action:Action.t
    -> produced_targets:unit Targets.Produced.t
    -> Digest.t Targets.Produced.t Fiber.t
end
