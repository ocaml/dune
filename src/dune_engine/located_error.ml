open Stdune

(* We can't just add a [Path.t option] field to [User_error.E] because that
   creates dependency cycles. *)
exception E of User_message.t * Path.t

let raise ?loc ?hints ~dir paragraphs =
  raise (E (User_error.make ?loc ?hints paragraphs, dir))
