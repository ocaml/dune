(** Error meant for humans *)

(** User errors are errors that users need to fix themselves in order
    to make progress. Since these errors are read by users, they should
    be simple to understand for people who are not familiar with the
    dune codebase.
*)
exception E of User_message.t

(** Styles that can be used inside error messages *)
module Style : sig
  type t =
    | Loc
    | Error
    | Warning
    | Kwd
    | Id
    | Prompt
    | Details
    | Ok
    | Debug
end

(** Raise a user error.  The arguments are interpreted in the same way
    as [User_message.make]. The first paragraph is prefixed with
    "Error:".  *)
val raise
  :  ?loc:Loc.t
  -> ?hints:Style.t Pp.t list
  -> Style.t Pp.t list
  -> _

(** Create a user error. *)
val make
  :  ?loc:Loc.t
  -> ?hints:Style.t Pp.t list
  -> Style.t Pp.t list
  -> User_message.t
