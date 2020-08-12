(** Scheduling *)

open! Stdune

(** [go ?log ?config fiber] runs the fiber until it terminates. *)
val go : ?config:Config.t -> (unit -> 'a Fiber.t) -> 'a

(** Runs [once] in a loop, executing [finally] after every iteration, even if
    Fiber.Never was encountered.

    If any source files change in the middle of iteration, it gets canceled. *)
val poll :
     ?config:Config.t
  -> once:(unit -> unit Fiber.t)
  -> finally:(unit -> unit)
  -> unit
  -> 'a

(** [with_job_slot f] waits for one job slot (as per [-j <jobs] to become
    available and then calls [f]. *)
val with_job_slot : (unit -> 'a Fiber.t) -> 'a Fiber.t

(** Wait for the following process to terminate *)
val wait_for_process : Pid.t -> Unix.process_status Fiber.t

(** Wait for dune cache to be disconnected. Drop any other event. *)
val wait_for_dune_cache : unit -> unit

val set_concurrency : int -> unit Fiber.t

(** Make the scheduler ignore next change to a certain file in watch mode.

    This is used with promoted files that are copied back to the source tree
    after generation *)
val ignore_for_watch : Path.t -> unit

(** Number of jobs currently running in the background *)
val running_jobs_count : unit -> int

(** Execute the given callback with current directory temporarily changed *)
val with_chdir : dir:Path.t -> f:(unit -> 'a) -> 'a

(** Notify the scheduler of a file to deduplicate from another thread *)
val send_dedup : Cache.caching -> Cache.File.t -> unit
