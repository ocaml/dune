open Import

module Local = struct
  module T = struct
    type t =
      { name : Lib_name.t
      ; loc : Loc.t
      ; src_dir : Path.Source.t
      }

    let compare a b =
      match Lib_name.compare a.name b.name with
      | Eq ->
        (match Path.Source.compare a.src_dir b.src_dir with
         | Eq -> Loc.compare a.loc b.loc
         | o -> o)
      | x -> x
    ;;

    let to_dyn { name; loc; src_dir; _ } =
      let open Dyn in
      record
        [ "name", Lib_name.to_dyn name
        ; "loc", Loc.to_dyn_hum loc
        ; "src_dir", Path.Source.to_dyn src_dir
        ]
    ;;

    let equal a b = Ordering.is_eq (compare a b)
  end

  include T
  include Comparable.Make (T)

  let make ~loc ~src_dir name = { name; loc; src_dir }
  let name t = t.name
  let loc t = t.loc
end

module T = struct
  type t =
    | External of (Loc.t * Lib_name.t)
    | Local of Local.t

  let compare a b =
    match a, b with
    | External (_, a), External (_, b) -> Lib_name.compare a b
    | Local a, Local b -> Local.compare a b
    | Local { loc = loc1; _ }, External (loc2, _)
    | External (loc1, _), Local { loc = loc2; _ } -> Loc.compare loc1 loc2
  ;;

  let to_dyn t =
    let open Dyn in
    match t with
    | External (_, lib_name) -> variant "External" [ Lib_name.to_dyn lib_name ]
    | Local t -> variant "Local" [ Local.to_dyn t ]
  ;;

  let equal a b = Ordering.is_eq (compare a b)
end

include T
include Comparable.Make (T)

let to_local_exn = function
  | Local t -> t
  | External (loc, name) ->
    Code_error.raise ~loc "Expected a Local library id" [ "name", Lib_name.to_dyn name ]
;;

let name = function
  | Local { name; _ } | External (_, name) -> name
;;

let loc = function
  | Local { loc; _ } | External (loc, _) -> loc
;;
