(** Scheduling *)

open! Stdune

type status_line_config =
  { message   : string option
  ; show_jobs : bool
  }

(** [go ?log ?config ?gen_status_line fiber] runs the following fiber until it
    terminates. [gen_status_line] is used to print a status line when [config.display =
    Progress]. *)
val go
  :  ?log:Log.t
  -> ?config:Config.t
  -> ?gen_status_line:(unit -> status_line_config)
  -> 'a Fiber.t
  -> 'a

(** Runs a fiber loop that looks like this (if cache_init is true, as default):
              /------------------\
              v                  |
    init --> once --> finally  --/

    If cache_init is false, every iteration reexecutes init instead of
    saving it.

    [~watch] should return after the first change to any of the project files.
*)
val poll
  :  ?log:Log.t
  -> ?config:Config.t
  -> ?cache_init:bool
  -> init:(unit -> unit Fiber.t)
  -> once:(unit -> unit Fiber.t)
  -> finally:(unit -> unit Fiber.t)
  -> watch:(unit -> unit Fiber.t)
  -> unit
  -> 'a

(** Wait for the following process to terminate *)
val wait_for_process : int -> Unix.process_status Fiber.t

(** Set the status line generator for the current scheduler *)
val set_status_line_generator : (unit -> status_line_config) -> unit Fiber.t

val set_concurrency : int -> unit Fiber.t

(** Scheduler information *)
type t

(** Wait until less tham [!Clflags.concurrency] external processes are running and return
    the scheduler information. *)
val wait_for_available_job : unit -> t Fiber.t

(** Logger *)
val log : t -> Log.t

(** Execute the given callback with current directory temporarily changed *)
val with_chdir : t -> dir:Path.t -> f:(unit -> 'a) -> 'a

(** Display mode for this scheduler *)
val display : t -> Config.Display.t

(** Print something to the terminal *)
val print : t -> string -> unit
