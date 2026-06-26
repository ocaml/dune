open Import

(** Diff two files that are expected not to match. *)
val print
  :  skip_trailing_cr:bool
  -> patch_back:Path.t option
  -> User_message.Diff_annot.t
  -> Path.t
  -> Path.t
  -> _ Fiber.t

(** Like [print], but displays the diff without raising. Used when the diff is
    going to be auto-promoted, in which case it is not an error. *)
val print_no_fail
  :  skip_trailing_cr:bool
  -> patch_back:Path.t option
  -> User_message.Diff_annot.t
  -> Path.t
  -> Path.t
  -> unit Fiber.t

module Diff : sig
  type t

  val print : t -> unit
end

val get : Path.t -> Path.t -> (Diff.t, User_message.t) result Fiber.t
