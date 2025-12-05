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

  let to_dyn { dir; name } =
    Dyn.record [ "name", Package_name.to_dyn name; "dir", Path.Source.to_dyn dir ]
  ;;
end

include T

let create ~name ~dir = { name; dir }
let hash { name; dir } = Tuple.T2.hash Package_name.hash Path.Source.hash (name, dir)
let name t = t.name

module C = Comparable.Make (T)
module Set = C.Set
module Map = C.Map
