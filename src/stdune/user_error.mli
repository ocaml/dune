(** Error meant for humans *)

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

(** An error that is meant to be shown to the user. If a location is
    provided, it will be included at the beginning of the error
    message.

    The various paragraphs are printed one after the other and all
    start at the beginning of a line. They are all wrapped inside a
    [Pp.box] and the first paragraph is prefixed with "Error: ".  *)
type t =
  { loc : Loc.t option
  ; paragraphs : Style.t Pp.t list
  }

exception E of t

val raise : ?loc:Loc.t -> Style.t Pp.t list -> _

val pp : t -> Style.t Pp.t

module Ansi_output : sig
  type config = (Style.t -> Ansi_color.Style.t list)

  (** Print to [stdout] (not thread safe) *)
  val print : ?config:config -> ?margin:int -> Style.t Pp.t -> unit

  (** Print to [stderr] (not thread safe) *)
  val prerr : ?config:config -> ?margin:int -> Style.t Pp.t -> unit
end
