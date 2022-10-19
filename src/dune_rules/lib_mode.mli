type t =
  | Ocaml of Ocaml.Mode.t
  | Melange

module Cm_kind : sig
  type t =
    | Ocaml of Ocaml.Cm_kind.t
    | Melange of Melange.Cm_kind.t

  val source : t -> Ocaml.Ml_kind.t

  val ext : t -> string

  val cmi : t -> t

  val to_dyn : t -> Dyn.t

  module Map : sig
    type cm_kind := t

    type 'a t =
      { ocaml : 'a Ocaml.Cm_kind.Dict.t
      ; melange : 'a Melange.Cm_kind.Map.t
      }

    val get : 'a t -> cm_kind -> 'a

    val make_all : 'a -> 'a t
  end
end

val of_cm_kind : Cm_kind.t -> t

module Map : sig
  type 'a t =
    { ocaml : 'a Ocaml.Mode.Dict.t
    ; melange : 'a
    }

  val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  module Set : sig
    type nonrec t = bool t

    val to_dyn : t -> Dyn.t

    val equal : t -> t -> bool
  end
end
