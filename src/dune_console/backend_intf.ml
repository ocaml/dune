open Stdune

module type S = sig
  val start : unit -> unit
  val print_user_message : User_message.t -> unit
  val set_status : User_message.Style.t Pp.t list -> unit
  val print_if_no_status : User_message.Style.t Pp.t list -> unit
  val reset : unit -> unit
  val reset_flush_history : unit -> unit
  val finish : unit -> unit
end

type t = (module S)
