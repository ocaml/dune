(** A message for the user *)

(** User messages are styled document that can be printed to the console or in
    the log file. *)

(** Symbolic styles that can be used inside messages. These styles are later
    converted to actual concrete styles depending on the output device. For
    instance, when printed to the terminal they are converted to ansi terminal
    styles ([Ansi_color.Style.t list] values). *)
module Style : sig
  type t =
    | Loc
    | Error
    | Warning
    | Kwd
    | Id
    | Prompt
    | Hint
    | Details
    | Ok
    | Debug
    | Success
    | Ansi_styles of Ansi_color.Style.t list
end

module Annots : sig
  include Univ_map.S

  (** The message has a location embed in the text. *)
  val has_embedded_location : unit Key.t

  (** The message needs a stack trace for clarity. *)
  val needs_stack_trace : unit Key.t
end

(** A user message.contents composed of an optional file location and a list of
    paragraphs.

    The various paragraphs will be printed one after the other and will all
    start at the beginning of a line. They are all wrapped inside a [Pp.box].

    When hints are provided, they are printed as last paragraphs and prefixed
    with "Hint:". Hints should give indication to the user for how to fix the
    issue.

    The [annots] field is intended to carry extra context for other,
    non-user-facing purposes (such as data for the RPC). *)
type t =
  { loc : Loc0.t option
  ; paragraphs : Style.t Pp.t list
  ; hints : Style.t Pp.t list
  ; annots : Annots.t
  }

val compare : t -> t -> Ordering.t

val equal : t -> t -> bool

val pp : t -> Style.t Pp.t

module Print_config : sig
  (** Associate ANSI terminal styles to symbolic styles *)
  type t = Style.t -> Ansi_color.Style.t list

  (** The default configuration *)
  val default : t
end

(** Construct a user message from a list of paragraphs.

    The first paragraph is prefixed with [prefix] inside the box. [prefix]
    should not end with a space as a space is automatically inserted by [make]
    if necessary. *)
val make :
     ?loc:Loc0.t
  -> ?prefix:Style.t Pp.t
  -> ?hints:Style.t Pp.t list
  -> ?annots:Annots.t
  -> Style.t Pp.t list
  -> t

(** Print to [stdout] (not thread safe) *)
val print : ?config:Print_config.t -> t -> unit

(** Print to [stderr] (not thread safe) *)
val prerr : ?config:Print_config.t -> t -> unit

(** Produces a "Did you mean ...?" hint *)
val did_you_mean : string -> candidates:string list -> Style.t Pp.t list

(** Produces a plain text representation of the error message, without the
    "Error: " prefix. *)
val to_string : t -> string

(** Returns [true] if the message has an explicit location or one embedded in
    the text. *)
val has_location : t -> bool

(** Returns [true] if the message's annotations contains
    [Annot.Has_embedded_location]. *)
val has_embedded_location : t -> bool

(** Returns [true] if the message's annotations contains
    [Annot.Needs_stack_trace]. *)
val needs_stack_trace : t -> bool
