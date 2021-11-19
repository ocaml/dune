module type S = Set_intf.S

module Make (Key : Map_intf.Key) (M : Map_intf.S with type key = Key.t) =
struct end
