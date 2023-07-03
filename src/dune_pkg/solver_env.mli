open! Stdune

module Flag : sig
  type t =
    [ `With_test
    | `With_doc
    ]

  val to_string : t -> string

  module Set : sig
    type flag := t

    type t

    val fold : t -> init:'a -> f:(flag -> 'a -> 'a) -> 'a
  end
end

type t = { flags : Flag.Set.t }

val decode : t Dune_lang.Decoder.t

val to_dyn : t -> Dyn.t

val equal : t -> t -> bool

val default : t
