open Import

module T = struct
  type t =
    { name : Package_name.t
    ; dir : Source_path.t
    }

  let compare { name; dir } pkg =
    match Package_name.compare name pkg.name with
    | Eq -> Source_path.compare dir pkg.dir
    | e -> e
  ;;

  let repr =
    Repr.record
      "package-id"
      [ Repr.field "name" Package_name.repr ~get:(fun t -> t.name)
      ; Repr.field "dir" Source_path.repr ~get:(fun t -> t.dir)
      ]
  ;;

  let to_dyn = Repr.to_dyn repr
end

include T

let create ~name ~dir = { name; dir }
let hash { name; dir } = Tuple.T2.hash Package_name.hash Source_path.hash (name, dir)
let name t = t.name

module C = Comparable.Make (T)
module Set = C.Set
module Map = C.Map
