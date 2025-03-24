(** Workspace-local and shared caches for rules. *)

open Import

(** The workspace-local cache consists of two components:

    - Build artifacts currently available in the build directory.

    - A database [_build/.db] that maps rule digests to their target digests.

    The database makes it possible to decide if the build directory contains up
    to date results for a given rule. *)
module Workspace_local : sig
  (** Check if the workspace-local cache contains up-to-date results for a rule
      using the information stored in the rule database. *)
  val lookup
    :  always_rerun:bool
    -> rule_digest:Digest.t
    -> targets:Targets.Validated.t
    -> env:Env.t
    -> build_deps:(Dep.Set.t -> Dep.Facts.t Memo.t)
    -> Digest.t Targets.Produced.t option Fiber.t

  (** Add a new record to the rule database. *)
  val store
    :  head_target:Path.Build.t
    -> rule_digest:Digest.t
    -> dynamic_deps_stages:(Dep.Set.t * Digest.t) list
    -> targets_digest:Digest.t
    -> unit
end

(** The shared cache is a separate directory that contains historical build
    artifacts produced in different workspaces. To restore results from the
    shared cache, Dune copes or hardlinks them into the build directory. *)
module Shared : Dune_cache.Shared.S
