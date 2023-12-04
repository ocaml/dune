open Import

type t =
  { ui : Ui.t
  ; toggle : unit -> unit
  }

val make
  :  helper_attr:A.t
  -> divider_attr:A.t
  -> help_screen_lines:string list
  -> width:int
  -> height:int
  -> t Lwd.t
