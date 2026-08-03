open Import

module T = struct
  type t =
    { name : Package_name.t
    ; dir : Path.Source.t
    }

  let compare { name; dir } pkg =
    match Package_name.compare name pkg.name with
    | Eq -> Path.Source.compare dir pkg.dir
    | e -> e
  ;;

  let repr =
    Repr.record
      "package-id"
      [ Repr.field "name" Package_name.repr ~get:(fun t -> t.name)
      ; Repr.field "dir" Path.Source.repr ~get:(fun t -> t.dir)
      ]
  ;;

  let to_dyn = Repr.to_dyn repr
end

include T

let create ~name ~dir = { name; dir }
let hash { name; dir } = Tuple.T2.hash Package_name.hash Path.Source.hash (name, dir)
let name t = t.name

module C = Comparable.Make (T)
module Set = C.Set
module Map = C.Map
