(** Simplified Async/Lwt like monad *)

type 'a t

val return : 'a -> 'a t
val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
val (>>|) : 'a t -> ('a -> 'b) -> 'b t

val both : 'a t -> 'b t -> ('a * 'b) t

val all : 'a t list -> 'a list t
val all_unit : unit t list -> unit t

val with_exn_handler : (unit -> 'a) -> handler:(exn -> Printexc.raw_backtrace -> unit) -> 'a

(** [run ?dir ?stdout_to prog args] spawns a sub-process and wait for its termination *)
val run
  :  ?dir:string
  -> ?stdout_to:string
  -> ?env:string array
  -> string
  -> string list
  -> unit t

(** Run a command and capture its output *)
val run_capture
  :  ?dir:string
  -> ?env:string array
  -> string
  -> string list
  -> string t
val run_capture_line
  :  ?dir:string
  -> ?env:string array
  -> string
  -> string list
  -> string t
val run_capture_lines
  :  ?dir:string
  -> ?env:string array
  -> string
  -> string list
  -> string list t

module Scheduler : sig
  val go : ?log:out_channel -> 'a t -> 'a
end
