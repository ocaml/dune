module Make (Key : Map.Key)
  : Ordered_intf.S with module Key = Key
