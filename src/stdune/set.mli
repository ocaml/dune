module type S = Set_intf.S

module Make(Elt : Comparable.S) : S with type elt = Elt.t
