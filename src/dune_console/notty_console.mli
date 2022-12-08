open Stdune

(** Pretty-printing to Notty images. *)

(** [image_of_user_message_style_pp pp] renders the pretty-printer [pp] to a
    Notty image, using an interpretation of the [User_message.Style.t] tags to
    [Notty.A.t]. *)
val image_of_user_message_style_pp : User_message.Style.t Pp.t -> Notty.image
