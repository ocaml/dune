module Make (Key : Map.Key) = struct
  module Map = Map.Make (Key)
  module Set = Set.Make (Key) (Map)
end
