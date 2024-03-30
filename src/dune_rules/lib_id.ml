open Import

module T = struct
  type t =
    { name : Lib_name.t
    ; loc : Loc.t
    ; src_dir : Path.t
    ; enabled_if : Blang.t
    }

  let compare a b =
    match Lib_name.compare a.name b.name with
    | Eq ->
      (match Path.compare a.src_dir b.src_dir with
       | Eq -> Loc.compare a.loc b.loc
       | o -> o)
    | x -> x
  ;;

  let to_dyn { name; loc; enabled_if; src_dir } =
    let open Dyn in
    record
      [ "name", Lib_name.to_dyn name
      ; "loc", Loc.to_dyn_hum loc
      ; "src_dir", Path.to_dyn src_dir
      ; "enabled_if", Blang.to_dyn enabled_if
      ]
  ;;

  let equal a b = Ordering.is_eq (compare a b)
end

include T
include Comparable.Make (T)

let external_ ~loc ~src_dir name = { name; loc; enabled_if = Blang.true_; src_dir }

let make ~loc ~src_dir ~enabled_if name =
  { name; loc; enabled_if; src_dir = Path.source src_dir }
;;

let name { name; _ } = name
let loc { loc; _ } = loc
