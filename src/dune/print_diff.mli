open! Stdune

val print : ?skip_trailing_cr:bool -> Path.t -> Path.t -> _ Fiber.t
(** Diff two files that are expected not to match. *)
