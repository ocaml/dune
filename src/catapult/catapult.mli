(** Output trace data to a file in catapult format.
    This format is compatible with [chrome://tracing]. *)

type t

val make : unit -> t

val close : t -> unit

val enable : t -> string -> unit

type event

val on_process_start : program:string -> args:string list -> event

val on_process_end : t -> event -> unit
