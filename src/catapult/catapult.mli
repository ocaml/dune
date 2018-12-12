(** Output trace data to a file in catapult format.
    This format is compatible with [chrome://tracing]. *)

(** The (mutable) state of reporters.
    It is basically an output channel. *)
type t

(** Create a new reporter.
    Initially, the reporter is in a disabled state where events are ignored and
    no trace file is written. *)
val make : unit -> t

(** Output trailing data to make the underlying file valid JSON, and close it. *)
val close : t -> unit

(** Enable tracing: open a trace file and further events will be logged into it.
    The file is only created when the first event is logged. It is necessary to
    call [close] on the reporter to make the file valid. *)
val enable : t -> string -> unit

type event

(** Prepare data related to the processus. This will capture the current time to
    compute the start and duration. *)
val on_process_start : t -> program:string -> args:string list -> event

(** Capture the current time and output a complete event. *)
val on_process_end : t -> event -> unit
