module type S = Set_intf.S

module Make (Key : Map_intf.Key) (M : Map.S with type key = Key.t) : sig end
[@@deprecated "Use Comparable.Make instead"]
