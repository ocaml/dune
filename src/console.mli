open! Stdune

val print : string -> unit

val print_user_message
  :  ?config:User_message.Print_config.t
  -> User_message.t
  -> unit

type status_line_config =
  { message   : User_message.Style.t Pp.t option
  ; show_jobs : bool
  }

val init : Config0.Display.t -> unit

(** / *)
(** Everything below this line requires [init] to have been called earlier. *)

val get_status_line_generator : unit -> (unit -> status_line_config)
val set_status_line_generator : (unit -> status_line_config) -> running_jobs:int -> unit

val update_status_line : running_jobs:int -> unit
val hide_status_line : unit -> unit
val display : unit -> Config0.Display.t
