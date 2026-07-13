open Import

(** Diff two files that are expected not to match. The optional
    source-promotion annotation is attached to the rendered diff output and
    errors. *)
val print
  :  ?promotion:User_message.Diff_annot.t
  -> skip_trailing_cr:bool
  -> patch_back:Path.t option
  -> Path.t
  -> Path.t
  -> _ Fiber.t

module Diff : sig
  type t

  val print : t -> unit
end

val get : Path.t -> Path.t -> (Diff.t, User_message.t) result Fiber.t
