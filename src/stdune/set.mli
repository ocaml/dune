module type S = Set_intf.S

module Make(Elt : Map.Key) : S with type elt = Elt.t
