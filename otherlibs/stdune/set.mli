module type S = Set_intf.S

module Make (Key : Map_intf.Key) (M : Map.S with type key = Key.t) :
  S with type elt = M.key and type 'a map = 'a M.t

module Of_map (Key : Map_intf.Key) (M : Map.S with type key = Key.t) :
  S with type elt = M.key and type 'a map = 'a M.t and type t = unit M.t
