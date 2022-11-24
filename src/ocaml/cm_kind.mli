open Stdune

type t =
  | Cmi
  | Cmo
  | Cmx

val compare : t -> t -> Ordering.t

val all : t list

val ext : t -> string

val source : t -> Ml_kind.t

val to_dyn : t -> Dyn.t

module Dict : sig
  type cm_kind := t

  type 'a t =
    { cmi : 'a
    ; cmo : 'a
    ; cmx : 'a
    }

  val get : 'a t -> cm_kind -> 'a

  val of_func : (cm_kind:cm_kind -> 'a) -> 'a t

  val make_all : 'a -> 'a t
end
