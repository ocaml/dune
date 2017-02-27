(** Simplified Async/Lwt like monad *)

open Import

type 'a t

val return : 'a -> 'a t
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
val (>>|) : 'a t -> ('a -> 'b) -> 'b t

val both : 'a t -> 'b t -> ('a * 'b) t

val all : 'a t list -> 'a list t
val all_unit : unit t list -> unit t

val with_exn_handler : (unit -> 'a) -> handler:(exn -> Printexc.raw_backtrace -> unit) -> 'a

(** How to handle sub-process failures *)
type ('a, 'b) failure_mode =
  | Strict : ('a, 'a) failure_mode
  (** Fail if the process exits with anything else than [0] *)
  | Accept : int list -> ('a, ('a, int) result) failure_mode
  (** Accept the following non-zero exit codes, and return [Error code] if the process
      exists with one of these codes. *)

(** [run ?dir ?stdout_to prog args] spawns a sub-process and wait for its termination *)
val run
  :  ?dir:string
  -> ?stdout_to:string
  -> ?env:string array
  -> (unit, 'a) failure_mode
  -> string
  -> string list
  -> 'a t

(** Run a command and capture its output *)
val run_capture
  :  ?dir:string
  -> ?env:string array
  -> (string, 'a) failure_mode
  -> string
  -> string list
  -> 'a t
val run_capture_line
  :  ?dir:string
  -> ?env:string array
  -> (string, 'a) failure_mode
  -> string
  -> string list
  -> 'a t
val run_capture_lines
  :  ?dir:string
  -> ?env:string array
  -> (string list, 'a) failure_mode
  -> string
  -> string list
  -> 'a t

module Scheduler : sig
  val go : ?log:out_channel -> 'a t -> 'a
end
