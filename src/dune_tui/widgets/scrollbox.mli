open Import

module State : sig
  type t

  val init : t
end

type t =
  { ui : Ui.t
  ; vscroll : dir:[ `Up | `Down ] -> unit
  ; hscroll : dir:[ `Left | `Right ] -> unit
  }

val make : State.t Lwd.var -> Ui.t Lwd.t -> t Lwd.t
