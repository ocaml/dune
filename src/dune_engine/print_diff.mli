open Import

(** Diff two files that are expected not to match. *)
val print :
     ?skip_trailing_cr:bool
  -> User_message.Annots.t
  -> Path.t
  -> Path.t
  -> _ Fiber.t

module Diff : sig
  type t

  val print : t -> unit
end

val get :
     ?skip_trailing_cr:bool
  -> User_message.Annots.t
  -> Path.t
  -> Path.t
  -> (Diff.t, User_message.t) result Fiber.t
