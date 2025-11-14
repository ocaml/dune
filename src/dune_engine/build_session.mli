open Import

(** Per-build context that holds state for a single build execution.
    This allows multiple builds to run concurrently without interfering. *)

module Progress : sig
  type t =
    { number_of_rules_discovered : int
    ; number_of_rules_executed : int
    ; number_of_rules_failed : int
    }

  val init : t
end

type t

(** Create a new build context with fresh state *)
val create : unit -> t

(** Get the unique ID for this build *)
val id : t -> Build_id.t

(** {1 Progress tracking} *)

val get_progress : t -> Progress.t Fiber.t
val set_progress : t -> Progress.t -> unit Fiber.t
val update_progress : t -> f:(Progress.t -> Progress.t) -> unit Fiber.t
val incr_rules_discovered : t -> unit Fiber.t
val incr_rules_executed : t -> unit Fiber.t
val incr_rules_failed : t -> unit Fiber.t

(** {1 Error tracking} *)

val get_errors : t -> Build_system_error.Set.t Fiber.t
val add_errors : t -> Build_system_error.t list -> unit Fiber.t
val reset_errors : t -> unit Fiber.t

(** {1 Finalize hooks} *)

(** Add a hook to run when this build completes *)
val add_finalize_hook : t -> (unit -> unit Fiber.t) -> unit

(** Run all finalize hooks for this build *)
val run_finalize_hooks : t -> unit Fiber.t

(** {1 Pending targets} *)

val add_pending_targets : t -> Targets.t -> unit
val remove_pending_targets : t -> Targets.t -> unit
val cleanup_pending_targets : t -> unit

(** {1 Debugging} *)

val to_dyn : t -> Dyn.t
