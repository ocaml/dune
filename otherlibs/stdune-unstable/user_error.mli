(** Error meant for humans *)

module Annot : sig
  type t

  module type S = sig
    type payload

    val make : payload -> t

    val check : t -> (payload -> 'a) -> (unit -> 'a) -> 'a
  end

  module Make (M : sig
    type payload

    val to_dyn : payload -> Dyn.t
  end) : S with type payload = M.payload

  (** The message has a location embed in the text. *)
  module Has_embedded_location : S with type payload = unit
end

(** User errors are errors that users need to fix themselves in order to make
    progress. Since these errors are read by users, they should be simple to
    understand for people who are not familiar with the dune codebase.

    The additional [Annot.t] is intended to carry extra context for other,
    non-user-facing purposes (such as data for the RPC). *)
exception E of User_message.t * Annot.t list

(** Raise a user error. The arguments are interpreted in the same way as
    [User_message.make]. The first paragraph is prefixed with "Error:". *)
val raise :
     ?loc:Loc0.t
  -> ?hints:User_message.Style.t Pp.t list
  -> ?annots:Annot.t list
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

(** Returns [true] if the message has an explicit location or one embed in the
    text. *)
val has_location : User_message.t -> Annot.t list -> bool

(** Returns [true] if the following list of annotations contains
    [Annot.Has_embedded_location]. *)
val has_embed_location : Annot.t list -> bool
