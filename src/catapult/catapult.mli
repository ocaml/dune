(** Output trace data to a file in catapult format. This format is compatible
    with [chrome://tracing]. *)

(** The (mutable) state of reporters. It is basically an output channel. *)
type t

val make : string -> t
(** Create a reporter: open a trace file and further events will be logged into
    it. It is necessary to call [close] on the reporter to make the file valid. *)

val fake : float ref -> Buffer.t -> t
(** Return a fake reporter that reads time in a reference and writes JSON
    objects to a buffer. *)

val close : t -> unit
(** Output trailing data to make the underlying file valid JSON, and close it. *)

type event

val on_process_start : t -> program:string -> args:string list -> event
(** Prepare data related to the processus. This will capture the current time to
    compute the start and duration. *)

val on_process_end : t -> event -> unit
(** Capture the current time and output a complete event. *)

val emit_counter : t -> string -> int -> unit
(** Emit a counter event. This is measuring the value of an integer variable. *)

val emit_gc_counters : t -> unit
(** Emit counter events for GC stats. *)
