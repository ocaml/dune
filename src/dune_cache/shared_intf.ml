open Import

(** The shared cache is a separate directory that contains historical build
    artifacts produced in different workspaces. To restore results from the
    shared cache, Dune copes or hardlinks them into the build directory. *)
module type S = sig
  (** Check if the shared cache contains results for a rule and decide whether
      to use these results or rerun the rule for a reproducibility check. *)
  val lookup
    :  can_go_in_shared_cache:bool
    -> rule_digest:Digest.t
    -> targets:Targets.Validated.t
    -> Digest.t Targets.Produced.t option Fiber.t

  (** This function performs the following steps:

      - Check that action produced all expected targets;

      - Compute their digests;

      - Remove write permissions from the targets;

      - Store results to the shared cache if needed. *)
  val examine_targets_and_store
    :  can_go_in_shared_cache:bool
    -> loc:Loc.t
    -> rule_digest:Digest.t
    -> should_remove_write_permissions_on_generated_files:bool
    -> action:(unit -> User_message.Style.t Pp.t)
    -> produced_targets:unit Targets.Produced.t
    -> Digest.t Targets.Produced.t Fiber.t
end
