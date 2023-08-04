module type S = Map_intf.S
module type Key = Map_intf.Key

module Make (Key : Key) : S with type key = Key.t
