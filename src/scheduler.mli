(** Scheduling *)

(** [go ?log t] runs the following fiber until it terminates. If it becomes clear that the
    fiber will never complete, for instance because of an uncaught exception, {!Never} is
    raised. *)
val go : ?log:Log.t -> 'a Fiber.t -> 'a

(** Wait for the following process to terminate *)
val wait_for_process : int -> Unix.process_status Fiber.t

(** Scheduler informations *)
type info =
  { log : Log.t
  (** Logger *)
  ; original_cwd : string
  (** Working directory at the time [go] was called *)
  }

(** Wait until less tham [!Clflags.concurrency] external processes are running and return
    the scheduler informations. *)
val wait_for_available_job : unit -> info Fiber.t
