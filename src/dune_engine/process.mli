(** Running external programs *)

open Import
module Action_output_on_success := Execution_parameters.Action_output_on_success
module Action_output_limit := Execution_parameters.Action_output_limit

module Failure_mode : sig
  (** How to handle sub-process failures *)
  type ('a, 'b) t =
    | Strict : ('a, 'a) t (** Fail if the process exits with anything else than [0] *)
    | Accept : int Predicate.t -> ('a, ('a, int) result) t
    (** Accept the following non-zero exit codes, and return [Error code] if
        the process exits with one of these codes. *)
    | Return : ('a, 'a * int) t (** Accept any error code and return it. *)
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

  val stdin : input t
  val null : 'a mode -> 'a t

  (** Return a buffered channel for this output. The channel is created lazily. *)
  val out_channel : output t -> out_channel

  (** Create a [t] representing redirecting the input or to a file or reading
      input from the file. The returned channel can only be used by a single
      call to {!run}. If you want to use it multiple times, you need to use
      [clone]. *)
  val file : Path.t -> ?perm:int -> 'a mode -> 'a t

  (** Call this when you no longer need this redirection *)
  val release : 'a t -> unit

  (** [multi_use t] returns a copy for which [release] does nothing *)
  val multi_use : 'a t -> 'a t
end

(** Why a Fiber.t was run.*)
type purpose =
  | Internal_job
  | Build_job of Targets.Validated.t option

(** Additional metadata attached to processes. The location and annotations will
    be attached to error messages. *)
type metadata =
  { loc : Loc.t option
  ; annots : User_message.Annots.t
  ; name : string option
    (** name when emitting stats. defaults to the basename of the executable *)
  ; categories : string list (** additional categories when emitting stats *)
  ; purpose : purpose
  }

val create_metadata
  :  ?loc:Loc.t
  -> ?annots:User_message.Annots.t
  -> ?name:string
  -> ?categories:string list
  -> ?purpose:purpose
  -> unit
  -> metadata

(* Dune overrides the TMPDIR for all running actions. At Jane Street, we change
   this behaviour by setting [set_temp_dir_when_running_actions = false]. *)
val set_temp_dir_when_running_actions : bool ref

(** [run ?dir ?stdout_to prog args] spawns a sub-process and wait for its
    termination. [stdout_to] [stderr_to] are released *)
val run
  :  ?dir:Path.t
  -> display:Display.t
  -> ?stdout_to:Io.output Io.t
  -> ?stderr_to:Io.output Io.t
  -> ?stdin_from:Io.input Io.t
  -> ?env:Env.t
  -> ?metadata:metadata
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
  -> ?metadata:metadata
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
  -> ?metadata:metadata
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
  -> ?metadata:metadata
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
  -> ?metadata:metadata
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
  -> ?metadata:metadata
  -> (string list, 'a) Failure_mode.t
  -> Path.t
  -> string list
  -> 'a Fiber.t
