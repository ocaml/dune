open Stdune
module Name = Package_name

module T = struct
  type t =
    { name : Name.t
    ; dir : Path.Source.t
    }

  let compare { name; dir } pkg =
    match Name.compare name pkg.name with
    | Eq -> Path.Source.compare dir pkg.dir
    | e -> e
  ;;

  let to_dyn { dir; name } =
    Dyn.record [ "name", Name.to_dyn name; "dir", Path.Source.to_dyn dir ]
  ;;
end

include T

let hash { name; dir } = Tuple.T2.hash Name.hash Path.Source.hash (name, dir)
let name t = t.name
let dir t = t.dir
let create ~name ~dir = { name; dir }

module C = Comparable.Make (T)
module Set = C.Set
module Map = C.Map
