open Import

(** Diff two files that are expected not to match. *)
val print
  :  sandbox:Process.Sandbox.t option
  -> skip_trailing_cr:bool
  -> patch_back:Path.t option
  -> User_message.Diff_annot.t
  -> Path.t
  -> Path.t
  -> _ Fiber.t

module Diff : sig
  type t

  val print : t -> unit
end

val get
  :  sandbox:Process.Sandbox.t option
  -> Path.t
  -> Path.t
  -> (Diff.t, User_message.t) result Fiber.t
