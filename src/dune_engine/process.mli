(** Running external programs *)

open Import
open Action_types
module Action_output_limit := Execution_parameters.Action_output_limit

module Failure_mode : sig
  type raw_status =
    | Exited of int
    | Signaled of Signal.t

  val exit_code_of_raw_status : raw_status -> int

  (** How to handle sub-process failures. This type controls the way in which
      the process we are running can fail. *)
  type ('a, 'b) t =
    | Strict : ('a, 'a) t (** Fail if the process exits with anything else than [0] *)
    | Accept : int Predicate.t -> ('a, ('a, int) result) t
    (** Accept the following non-zero exit codes, and return [Error code] if
        the process exits with one of these codes. *)
    | Return : ('a, 'a * int) t (** Accept any error code and return it. *)
    | Return_raw : ('a, 'a * raw_status) t
    (** Return any exit status without rendering a Dune process error. *)
    | Timeout :
        { timeout : Time.Span.t option
        ; failure_mode : ('a, 'b) t
        }
        -> ('a, ('b, [ `Timed_out ]) result) t
    (** In addition to the [failure_mode], finish early if [timeout]
        was reached. *)
end

module Io : sig
  (** Where to redirect stdout/stderr/stdin *)
  type input = Input

  type output = Output

  type 'a mode =
    | In : input mode
    | Out : output mode

  type 'a t

  val make_stdout
    :  output_on_success:Action_output_on_success.t
    -> output_limit:Action_output_limit.t
    -> output t

  val stderr : output t

  val make_stderr
    :  output_on_success:Action_output_on_success.t
    -> output_limit:Action_output_limit.t
    -> output t

  (** Inherited output descriptors that bypass Dune's output capture. *)
  val inherit_stdout : output t

  val inherit_stderr : output t
  val inherit_stdin : input t
  val stdin : input t
  val null : 'a mode -> 'a t

  (** Return a buffered channel for this output. The channel is created lazily. *)
  val out_channel : output t -> out_channel

  (** Create a [t] representing redirecting the input or to a file or reading
      input from the file. The returned channel can only be used by a single
      call to {!run}. If you want to use it multiple times, you need to use
      [clone]. *)
  val file : Path.t -> ?perm:Permissions.Mode.t -> 'a mode -> 'a t

  (** Call this when you no longer need this redirection *)
  val release : 'a t -> unit

  (** [multi_use t] returns a copy for which [release] does nothing *)
  val multi_use : 'a t -> 'a t
end

module Build : sig
  type t

  val create
    :  action_runner:Action_runner.t option
    -> run_id:Run_id.t
    -> cancellation:Fiber.Cancel.t
    -> t

  val run_id : t -> Run_id.t
  val cancellation : t -> Fiber.Cancel.t
  val action_runner : t -> Action_runner.t option
  val get : unit -> t option
  val with_ : t -> (unit -> 'a Fiber.t) -> 'a Fiber.t
  val cancel_current : unit -> unit Fiber.t
end

(** Execute a [Process_runner.request] locally without taking another scheduler job
    slot. The caller must already account for scheduler concurrency. This is
    intended for the external worker implementation. *)
val exec_locally
  :  build:Build.t
  -> Process_runner.request
  -> Process_runner.response Fiber.t

(** [run ?dir ?stdout_to prog args] spawns a sub-process and wait for its
    termination. [stdout_to] [stderr_to] are released *)
val run
  :  ?dir:Path.t
  -> display:Display.t
  -> ?stdout_to:Io.output Io.t
  -> ?stderr_to:Io.output Io.t
  -> ?stdin_from:Io.input Io.t
  -> ?env:Env.t
  -> ?metadata:Process_metadata.t
  -> ?build:Build.t
  -> (unit, 'a) Failure_mode.t
  -> Path.t
  -> string list
  -> 'a Fiber.t

val run_with_times
  :  ?dir:Path.t
  -> display:Display.t
  -> ?stdout_to:Io.output Io.t
  -> ?stderr_to:Io.output Io.t
  -> ?stdin_from:Io.input Io.t
  -> ?env:Env.t
  -> ?metadata:Process_metadata.t
  -> ?build:Build.t
  -> (Proc.Times.t, 'a) Failure_mode.t
  -> Path.t
  -> string list
  -> 'a Fiber.t

(** Run a command and capture its output *)
val run_capture
  :  ?dir:Path.t
  -> display:Display.t
  -> ?stderr_to:Io.output Io.t
  -> ?stdin_from:Io.input Io.t
  -> ?env:Env.t
  -> ?metadata:Process_metadata.t
  -> ?build:Build.t
  -> (string, 'a) Failure_mode.t
  -> Path.t
  -> string list
  -> 'a Fiber.t

val run_capture_line
  :  ?dir:Path.t
  -> display:Display.t
  -> ?stderr_to:Io.output Io.t
  -> ?stdin_from:Io.input Io.t
  -> ?env:Env.t
  -> ?metadata:Process_metadata.t
  -> ?build:Build.t
  -> (string, 'a) Failure_mode.t
  -> Path.t
  -> string list
  -> 'a Fiber.t

val run_capture_lines
  :  ?dir:Path.t
  -> display:Display.t
  -> ?stderr_to:Io.output Io.t
  -> ?stdin_from:Io.input Io.t
  -> ?env:Env.t
  -> ?metadata:Process_metadata.t
  -> ?build:Build.t
  -> (string list, 'a) Failure_mode.t
  -> Path.t
  -> string list
  -> 'a Fiber.t

val run_capture_zero_separated
  :  ?dir:Path.t
  -> display:Display.t
  -> ?stderr_to:Io.output Io.t
  -> ?stdin_from:Io.input Io.t
  -> ?env:Env.t
  -> ?metadata:Process_metadata.t
  -> ?build:Build.t
  -> (string list, 'a) Failure_mode.t
  -> Path.t
  -> string list
  -> 'a Fiber.t

(** [run_inherit_std_in_out] differs from the other [run] functions in the
    followings ways:

    - The process group ID is inherited by the parent process rather than
    creating a new one.

    - The input and output file descriptors are the standard ones.

    This version is intended for running external processes at the end of a
    build such as the ones spawned with "dune exec". *)
val run_inherit_std_in_out
  :  ?dir:Path.t
  -> ?env:Env.t
  -> Path.t
  -> string list
  -> int Fiber.t

(** Like [run_inherit_std_in_out], but preserves whether the process exited or
    was terminated by a signal. *)
val run_inherit_std_in_out_raw
  :  ?dir:Path.t
  -> ?env:Env.t
  -> Path.t
  -> string list
  -> Failure_mode.raw_status Fiber.t
