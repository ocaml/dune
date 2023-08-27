open Import

module Tab : sig
  type t =
    { title : string
    ; ui : unit -> Ui.t Lwd.t
    }
end

type t =
  { ui : Ui.t
  ; title_attr : A.t
  ; set_index : int -> unit
  }

val make : title_attr:A.t -> Tab.t list -> t Lwd.t
