open Stdune

(** [serialize stat ~path] serializes the GC statistics [stat] to the file
    [path]. *)
val serialize : Stdlib.Gc.stat -> path:Path.t -> unit

val decode : Stdlib.Gc.stat Dune_sexp.Decoder.t
