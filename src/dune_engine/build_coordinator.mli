(** Coordinates multiple concurrent builds. Allows RPC and watch mode builds
    to execute simultaneously while maintaining correctness. *)

type t

(** Create a new build coordinator *)
val create : unit -> t

(** Submit a build request. The function receives a fresh Build_session.t
    and should execute the build using that context. Multiple calls to submit
    can run concurrently. *)
val submit : t -> f:(Build_session.t -> 'a Fiber.t) -> 'a Fiber.t

(** Get list of currently active build IDs *)
val active_build_ids : t -> Build_id.t list Fiber.t

(** Get the context for a specific build (if still active) *)
val get_build_context : t -> Build_id.t -> Build_session.t option Fiber.t

(** Get all active build contexts *)
val get_all_contexts : t -> (Build_id.t * Build_session.t) list Fiber.t

(** Get progress for all active builds *)
val get_all_progress : t -> (Build_id.t * Build_session.Progress.t) list Fiber.t

(** Get errors for all active builds *)
val get_all_errors : t -> (Build_id.t * Build_system_error.Set.t) list Fiber.t

(** Check if there are any active builds *)
val has_active_builds : t -> bool Fiber.t

(** {1 Debugging} *)

val to_dyn : t -> Dyn.t
