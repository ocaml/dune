module type S = sig
  type state

  val done_status_line
    :  complete:int
    -> remaining:int
    -> failed:int
    -> Stdune.User_message.Style.t Pp.t

  val render : state -> unit
end
