(** Special functions during bootstrap *)

open Stdune

(** Treat the following path as if it was declared as a data only path
    in a [dune] file. *)
val data_only_path : Path.Source.t -> bool
