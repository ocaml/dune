module RGB8 : sig
  (** 8 bit RGB color *)
  type t

  (** [RGB8.to_int t] returns the [int] value of [t] as an 8 bit integer. *)
  val to_int : t -> int
end

module RGB24 : sig
  (** 24 bit RGB color *)
  type t

  (** [RGB24.red t] returns the red component of [t] *)
  val red : t -> int

  (** [RGB24.green t] returns the green component of [t] *)
  val green : t -> int

  (** [RGB24.blue t] returns the blue component of [t] *)
  val blue : t -> int
end

module Style : sig
  (** ANSI terminal styles *)
  type t =
    [ `Fg_default
    | `Fg_black
    | `Fg_red
    | `Fg_green
    | `Fg_yellow
    | `Fg_blue
    | `Fg_magenta
    | `Fg_cyan
    | `Fg_white
    | `Fg_bright_black
    | `Fg_bright_red
    | `Fg_bright_green
    | `Fg_bright_yellow
    | `Fg_bright_blue
    | `Fg_bright_magenta
    | `Fg_bright_cyan
    | `Fg_bright_white
    | `Fg_8_bit_color of RGB8.t
    | `Fg_24_bit_color of RGB24.t
    | `Bg_default
    | `Bg_black
    | `Bg_red
    | `Bg_green
    | `Bg_yellow
    | `Bg_blue
    | `Bg_magenta
    | `Bg_cyan
    | `Bg_white
    | `Bg_bright_black
    | `Bg_bright_red
    | `Bg_bright_green
    | `Bg_bright_yellow
    | `Bg_bright_blue
    | `Bg_bright_magenta
    | `Bg_bright_cyan
    | `Bg_bright_white
    | `Bg_8_bit_color of RGB8.t
    | `Bg_24_bit_color of RGB24.t
    | `Bold
    | `Dim
    | `Italic
    | `Underline
    ]

  val to_dyn : t -> Dyn.t

  (** Ansi escape sequence that set the terminal style to exactly these styles *)
  val escape_sequence : t list -> string
end

val make_printer :
  bool Lazy.t -> Format.formatter -> (Style.t list Pp.t -> unit) Staged.t

(** Print to [Format.std_formatter] *)
val print : Style.t list Pp.t -> unit

(** Print to [Format.err_formatter] *)
val prerr : Style.t list Pp.t -> unit

(** Whether [stdout]/[stderr] support colors *)
val stdout_supports_color : bool Lazy.t

val stderr_supports_color : bool Lazy.t

val output_is_a_tty : bool Lazy.t

(** Filter out escape sequences in a string *)
val strip : string -> string

(** Parse a string containing ANSI escape sequences *)
val parse : string -> Style.t list Pp.t
