module type S = Hashtbl_intf.S

module Make (Key : sig
    include Hashable.S

    val to_dyn : t -> Dyn.t
  end) : S with type key = Key.t
