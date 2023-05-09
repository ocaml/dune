open Stdune

(** [User_message_to_image.pp ~attr pp] converts a pretty-printer [pp] to a
    [Notty.image] using the given [attr] for styling. *)
val pp : ?attr:Notty.attr -> User_message.Style.t Pp.t -> Notty.image
