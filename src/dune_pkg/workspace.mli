open Import

module Repository : sig
  type t

  val opam_url : t -> Loc.t * OpamUrl.t
  val hash : t -> int
  val to_dyn : t -> Dyn.t
  val equal : t -> t -> bool
  val upstream : t
  val overlay : t
  val decode : t Decoder.t

  module Name : sig
    type t

    val equal : t -> t -> bool
    val pp : t -> 'a Pp.t

    include Stringlike with type t := t
    include Comparable_intf.S with type key := t
  end

  val name : t -> Name.t
end
