open! Stdune

(** Diff two files that are expected not to match. *)
val print :
     ?skip_trailing_cr:bool
  -> User_message.Annots.t
  -> Path.t
  -> Path.t
  -> _ Fiber.t
