module T = struct
  type t = StringLabels.t

  module Include = struct
    let compare a b = Ordering.of_int (Caml.String.compare a b)
    let equal (x : t) (y : t) = x = y
    let hash (s : t) = Caml.Hashtbl.hash s
  end

  include Include
end

module Set = Set.Make(T)
module Map = Map.Make(T)
