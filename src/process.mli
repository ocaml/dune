(** Running external programs *)

open Import

type accepted_codes =
  | These of int list
  | All

(** How to handle sub-process failures *)
type ('a, 'b) failure_mode =
  | Strict : ('a, 'a) failure_mode
  (** Fail if the process exits with anything else than [0] *)
  | Accept : accepted_codes -> ('a, ('a, int) result) failure_mode
  (** Accept the following non-zero exit codes, and return [Error
      code] if the process exists with one of these codes. *)

module Output : sig
  (** Where to redirect stdout/stderr *)
  type t

  val stdout : t
  val stderr : t

  (** Create a [t] representing redirecting the output to a file. The
      returned output can only be used by a single call to {!run}. If
      you want to use it multiple times, you need to use [clone]. *)
  val file : Path.t -> t

  (** Call this when you no longer need this output *)
  val release : t -> unit

  (** Return a buffered channel for this output. The channel is
      created lazily. *)
  val channel : t -> out_channel

  (** [multi_use t] returns a copy for which [release] does nothing *)
  val multi_use : t -> t
end

(** Why a Fiber.t was run *)
type purpose =
  | Internal_job
  | Build_job of Path.Set.t

(** [run ?dir ?stdout_to prog args] spawns a sub-process and wait for its termination *)
val run
  :  ?dir:Path.t
  -> ?stdout_to:Output.t
  -> ?stderr_to:Output.t
  -> env:Env.t
  -> ?purpose:purpose
  -> (unit, 'a) failure_mode
  -> Path.t
  -> string list
  -> 'a Fiber.t

(** Run a command and capture its output *)
val run_capture
  :  ?dir:Path.t
  -> ?stderr_to:Output.t
  -> env:Env.t
  -> ?purpose:purpose
  -> (string, 'a) failure_mode
  -> Path.t
  -> string list
  -> 'a Fiber.t
val run_capture_line
  :  ?dir:Path.t
  -> ?stderr_to:Output.t
  -> env:Env.t
  -> ?purpose:purpose
  -> (string, 'a) failure_mode
  -> Path.t
  -> string list
  -> 'a Fiber.t
val run_capture_lines
  :  ?dir:Path.t
  -> ?stderr_to:Output.t
  -> env:Env.t
  -> ?purpose:purpose
  -> (string list, 'a) failure_mode
  -> Path.t
  -> string list
  -> 'a Fiber.t
