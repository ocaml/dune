module Color : sig
  type t =
    | Default
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
    | Bright_black
    | Bright_red
    | Bright_green
    | Bright_yellow
    | Bright_blue
    | Bright_magenta
    | Bright_cyan
    | Bright_white
end

module Style : sig
  type t =
    | Fg of Color.t
    | Bg of Color.t
    | Bold
    | Underlined
end

module Render : Pp.Renderer.S
  with type tag = Style.t list
