open Stdune

(** [serialize stat ~path] serializes the GC statistics [stat] to the file
    [path]. *)
val serialize : Stdlib.Gc.stat -> path:Path.t -> unit

val decode : Stdlib.Gc.stat Dune_sexp.Decoder.t

(** [event stat] returns a Chrome trace event for the GC statistics [stat]. *)
val event : Stdlib.Gc.stat -> Chrome_trace.Event.t
