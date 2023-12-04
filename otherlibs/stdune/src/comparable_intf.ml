module type S = sig
  type key

  module Map : Map_intf.S with type key = key
  module Set : Set_intf.S with type elt = key and type 'a map = 'a Map.t
end
