module type S = sig
  module Key : Map.Key

  module Map : Map_intf.S with type key = Key.t

  module Set : Set_intf.S with type elt = Key.t and type 'a map = 'a Map.t
end
