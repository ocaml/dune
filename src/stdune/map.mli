module type S = Map_intf.S

module Make(Key : Comparable.S) : S with type key = Key.t
