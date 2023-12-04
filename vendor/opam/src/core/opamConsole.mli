(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Console output, ANSI color, logging and user querying *)

(** Global configuration parameters (read from OpamGlobalConfig, and the
    environment when necessary) *)

val debug: unit -> bool
val verbose: unit -> bool
val color: unit -> bool
val utf8: unit -> bool
val utf8_extended: unit -> bool
val disp_status_line: unit -> bool

(** General text formatting *)

(** Settable attributes for ANSI terminal output. Nesting is generally not
    handled. *)
type text_style =
  [ `black
  | `blue
  | `bold
  | `crossed
  | `cyan
  | `green
  | `magenta
  | `red
  | `underline
  | `white
  | `yellow ]

(** Helper coloring functions. Returns the string unchanged if color is
    disabled *)
val colorise : text_style -> string -> string
val colorise' : text_style list -> string -> string
val acolor : text_style -> unit -> string -> string
val acolor_w : int -> text_style -> Format.formatter -> string -> unit

module Symbols : sig
  val rightwards_arrow : Uchar.t
  val box_drawings_light_down_and_right : Uchar.t
  val box_drawings_light_horizontal : Uchar.t
  val box_drawings_light_vertical : Uchar.t
  val box_drawings_light_up_and_right : Uchar.t
  val box_drawings_light_right : Uchar.t
  val circled_division_slash : Uchar.t
  val asterisk_operator : Uchar.t
  val north_east_arrow : Uchar.t
  val south_east_arrow : Uchar.t
  val clockwise_open_circle_arrow : Uchar.t
  val greek_small_letter_lambda : Uchar.t
  val latin_capital_letter_o_with_stroke : Uchar.t
  val six_pointed_black_star : Uchar.t
  val upwards_arrow : Uchar.t
  val downwards_arrow : Uchar.t
  val up_down_arrow : Uchar.t
  val downwards_double_arrow : Uchar.t
  val downwards_black_arrow : Uchar.t
  val black_down_pointing_triangle : Uchar.t
end

val utf8_symbol:
  Uchar.t -> ?alternates:Uchar.t list -> string -> string

(** Logging *)

(** Timers, only active when debug is on. Returns the time between the
    application to each argument, in seconds *)
val timer : unit -> unit -> float

(** [log section ~level fmt args]. Used for debug messages, default
    level is 1 *)
val log : string -> ?level:int -> ('a, Format.formatter, unit) format -> 'a

(** Helper to pass stringifiers to log (use [log "%a" (slog to_string) x]
    rather than [log "%s" (to_string x)] to avoid costly unneeded
    stringifications *)
val slog : ('a -> string) -> Format.formatter -> 'a -> unit

val error : ('a, unit, string, unit) format4 -> 'a
val warning : ('a, unit, string, unit) format4 -> 'a
val note : ('a, unit, string, unit) format4 -> 'a

(** Message without prefix, reformat or newline, to stderr (useful to continue
    error messages without repeating "[ERROR]") *)
val errmsg : ('a, unit, string, unit) format4 -> 'a

val error_and_exit :
  OpamStd.Sys.exit_reason -> ('a, unit, string, 'b) format4 -> 'a
val msg : ('a, unit, string, unit) format4 -> 'a
val formatted_msg : ?indent:int -> ('a, unit, string, unit) format4 -> 'a
val header_msg : ('a, unit, string, unit) format4 -> 'a
val header_error :
  ('a, unit, string, ('b, unit, string, unit) format4 -> 'b) format4 -> 'a

(** Erase the current line on stdout (doesn't flush stdout) *)
val carriage_delete: unit -> unit

(** Display a dynamic status line to stdout, that will be erased on next call.
    The message should not be wider than screen nor contain newlines. Use
    {!clear_status} when the status line should be erased. *)
val status_line : ('a, unit, string, unit) format4 -> 'a

(** Erase the status line and restore the cursor to the start of the line *)
val clear_status : unit -> unit

(** Show a prompt and wait for the user to press anything. *)
val pause: ('a, unit, string, unit) format4 -> 'a

(** Ask the user to press Y/y/N/n to continue (returns a boolean).
    Defaults to true (yes) if unspecified.
    If [require_unsafe_yes] is true, it automatically answer yes to the
    question if automatic answering is set to [`unsafe_yes] ; otherwise it will
    prompt and wait user input if it is set [`all_yes] (interactive). Its
    default is false. *)
val confirm:
  ?require_unsafe_yes:bool -> ?default:bool ->
  ('a, unit, string, bool) format4 -> 'a

(** Prompts the user with multiple numbered choices [(answer, message)].

    [unsafe_yes], [yes] are the options to choose if the corresponding global
    options are set.
    [no] is the option to choose otherwise, when non interactive, on [escape].
    [default] is the option to choose on an active empty input ("\n").
    Max 9 options. *)
val menu:
  ?default:'a -> ?unsafe_yes:'a -> ?yes:'a -> no:'a ->
  options:('a * string) list ->
  ('b, unit, string, 'a) format4 -> 'b

(** Read some input from the user (returns a string option) *)
val read: ('a, unit, string, string option) format4 -> 'a

(** Prints a table; generally called on tables passed through [align_table].
    The default [cut] is to wrap on stdout, stderr, keep as-is otherwise.
    [`Wrap sep] prepends [sep] on wrapped lines *)
val print_table:
  ?cut:[`Wrap of string | `Truncate | `None] -> out_channel -> sep:string ->
  string list list -> unit

(** Tree printing *)
module Tree : sig
  type 'elt t

  val value: 'elt t -> 'elt
  val children: 'elt t -> 'elt t list

  (** Creates a tree node. *)
  val create: ?children:'a t list -> 'a -> 'a t

  (** The symbols to be used in the tree printer. *)
  type symbols = {
    vert: string; (** | *)
    hor:  string; (** - *)
    tee:  string; (** |- *)
    hook: string; (** '- *)
  }

  (** Returns UTF8 or ASCII tree symbols depending on [utf8 ()]. *)
  val get_default_symbols: unit -> symbols

  (** Prints the given tree as a Unicode/ASCII art.
      @param printer may return a multi-line string, but should not return an
      empty string. *)
  val print: ?symbols:symbols -> printer:('a -> string) -> 'a t -> unit
end
