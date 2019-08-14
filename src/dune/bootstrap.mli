(** Special functions during bootstrap *)

open Stdune

(** Whether we're currently bootstrapping [dune] *)
val bootstrapping : bool

(** Treat the following path as if it was declared as a data only path in a
  [dune] file. *)
val data_only_path : Path.Source.t -> bool
