open Import

(** Miscellaneous drawing utilities for [Notty.I.t]. *)

(** [Drawing.pp_to_image pp] converts a [pp] to a [I.t] converting the
    [User_message.Style.t] tags into appropriate Notty [A.t]s. *)
val pp_to_image : User_message.Style.t Pp.t -> I.t

(** [horizontal_rule ~attr ~w] draws a horizontal line with [attr] of length [w]. *)
val horizontal_rule : attr:A.t -> w:int -> I.t

(** [box_with_title ~attr ~title ~title_attr img] draws a bordered box around the [img]
    which has [attr] as a style. It also has a [title] at the top with [title_attr].

    The box is drawn straight over the top of the image, so make sure to [I.pad] the
    outside. *)
val box_with_title : attr:A.t -> title:string -> title_attr:A.t -> I.t -> I.t

module Unicode : sig
  (** Unicode constants useful for drawing. *)

  (** ᚛ U+169B *)
  val ogham_feather_mark : Uchar.t

  (** ᚜ U+169C *)
  val ogham_reversed_feather_mark : Uchar.t

  (** ― U+2015 *)
  val horizontal_bar : Uchar.t

  (** ═ U+2550 *)
  val box_drawings_double_horizontal : Uchar.t

  (** ║ U+2551 *)
  val box_drawings_double_vertical : Uchar.t

  (** ╔ U+2554 *)
  val box_drawings_double_down_and_right : Uchar.t

  (** ╗ U+2557 *)
  val box_drawings_double_down_and_left : Uchar.t

  (** ╚ U+255A *)
  val box_drawings_double_up_and_right : Uchar.t

  (** ╝ U+255D *)
  val box_drawings_double_up_and_left : Uchar.t

  (** ╞ U+255E *)
  val box_drawings_vertical_single_and_right_double : Uchar.t

  (** ╡ U+2561 *)
  val box_drawings_vertical_single_and_left_double : Uchar.t
end
