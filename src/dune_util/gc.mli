open Stdune

(** [to_sexp stat] converts the GC statistics [stat] to an S-expression. *)
val to_sexp : Stdlib.Gc.stat -> Sexp.t

(** [serialize stat ~path] serializes the GC statistics [stat] to the file
    [path]. *)
val serialize : Stdlib.Gc.stat -> path:Path.t -> unit
