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
    | Details
    | Ok
    | Debug
    | Success
    | Ansi_styles of Ansi_color.Style.t list
end

(** A user message.contents composed of an optional file location and a list of
    paragraphs.

    The various paragraphs will be printed one after the other and will all
    start at the beginning of a line. They are all wrapped inside a [Pp.box].

    When hints are provided, they are printed as last paragraphs and prefixed
    with "Hint:". Hints should give indication to the user for how to fix the
    issue. *)
type t =
  { loc : Loc0.t option
  ; paragraphs : Style.t Pp.t list
  ; hints : Style.t Pp.t list
  }

val pp : t -> Style.t Pp.t

module Print_config : sig
  (** Associate ANSI terminal styles to symbolic styles *)
  type t = Style.t -> Ansi_color.Style.t list

  val default : t
  (** The default configuration *)
end

val make :
     ?loc:Loc0.t
  -> ?prefix:Style.t Pp.t
  -> ?hints:Style.t Pp.t list
  -> Style.t Pp.t list
  -> t
(** Construct a user message from a list of paragraphs.

    The first paragraph is prefixed with [prefix] inside the box. [prefix]
    should not end with a space as a space is automatically inserted by [make]
    if necessary. *)

val print : ?config:Print_config.t -> t -> unit
(** Print to [stdout] (not thread safe) *)

val prerr : ?config:Print_config.t -> t -> unit
(** Print to [stderr] (not thread safe) *)

val did_you_mean : string -> candidates:string list -> Style.t Pp.t list
(** Produces a "Did you mean ...?" hint *)

val to_string : t -> string
(** Produces a plain text representation of the error message, without the
    "Error: " prefix. *)
