(** Workspace-local and shared caches for rules. *)

open Import

(** The workspace-local cache consists of two components:

    - Build artifacts currently available in the build directory.

    - The rule records in the workspace cache [_build/.db], which map rule
      digests to their target digests.

    These records make it possible to decide if the build directory contains up
    to date results for a given rule. *)
module Workspace_local : sig
  type hit =
    { produced_targets : Digest.t Targets.Produced.t
    ; dynamic_deps_stages : (Dep.Set.t * Digest.t) list
    }

  (** Look up cached results. The caller must validate [dynamic_deps_stages]
      before reusing [produced_targets]. *)
  val lookup
    :  always_rerun:bool
    -> rule_digest:Digest.t
    -> targets:Targets.Validated.t
    -> hit option

  (** Add a new record to the rule database. *)
  val store
    :  targets:Digest.t Targets.Produced.t
    -> head_target:Path.Build.t
    -> rule_digest:Digest.t
    -> dynamic_deps_stages:(Dep.Set.t * Digest.t) list
    -> targets_digest:Digest.t
    -> unit

  val remove : Targets.Validated.t -> unit
  val remove_target : Path.Build.t -> unit
  val remove_subtree : Path.Build.t -> unit
end
