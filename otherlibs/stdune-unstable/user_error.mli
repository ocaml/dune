(** Error meant for humans *)

module Annot : sig
  type t = ..
end

(** User errors are errors that users need to fix themselves in order to make
    progress. Since these errors are read by users, they should be simple to
    understand for people who are not familiar with the dune codebase.

    The additional [Annot.t] is intended to carry extra context for other,
    non-user-facing purposes (such as data for the RPC). *)
exception E of User_message.t * Annot.t option

(** Raise a user error. The arguments are interpreted in the same way as
    [User_message.make]. The first paragraph is prefixed with "Error:". *)
val raise :
     ?loc:Loc0.t
  -> ?hints:User_message.Style.t Pp.t list
  -> ?annot:Annot.t
  -> User_message.Style.t Pp.t list
  -> _

(** Create a user error. *)
val make :
     ?loc:Loc0.t
  -> ?hints:User_message.Style.t Pp.t list
  -> User_message.Style.t Pp.t list
  -> User_message.t

(** The "Error:" prefix *)
val prefix : User_message.Style.t Pp.t
