open! Stdune

type t =
  | String of string
  | Dir of Path.t
  | Path of Path.t

let compare x y =
  match (x, y) with
  | String x, String y -> String.compare x y
  | String _, _ -> Lt
  | _, String _ -> Gt
  | Dir x, Dir y -> Path.compare x y
  | Dir _, _ -> Lt
  | _, Dir _ -> Gt
  | Path x, Path y -> Path.compare x y

let equal x y = Ordering.is_eq (compare x y)

let to_dyn = function
  | String s -> Dyn.variant "string" [ String s ]
  | Path p -> Dyn.variant "path" [ Path.to_dyn p ]
  | Dir p -> Dyn.variant "dir" [ Path.to_dyn p ]

let string_of_path ~dir p = Path.reach ~from:dir p

let to_string t ~dir =
  match t with
  | String s -> s
  | Dir p | Path p -> string_of_path ~dir p

let compare_vals ~dir x y =
  match (x, y) with
  | String x, String y -> String.compare x y
  | (Path x | Dir x), (Path y | Dir y) -> Path.compare x y
  | String x, (Path _ | Dir _) -> String.compare x (to_string ~dir y)
  | (Path _ | Dir _), String y -> String.compare (to_string ~dir x) y

let to_path ?error_loc t ~dir =
  match t with
  | String s -> Path.relative ?error_loc dir s
  | Dir p | Path p -> p

let to_path_in_build_or_external ?error_loc t ~dir =
  match t with
  | String s -> Path.relative_to_source_in_build_or_external ?error_loc ~dir s
  | Dir p | Path p ->
    if Path.is_in_source_tree p then
      Code_error.raise ?loc:error_loc
        "to_path_in_build_or_external got a file in source directory"
        [ ("path", Path.to_dyn p) ];
    p

module L = struct
  let to_dyn t = Dyn.List (List.map t ~f:to_dyn)

  let to_strings t ~dir = List.map t ~f:(to_string ~dir)

  let compare_vals ~dir = List.compare ~compare:(compare_vals ~dir)

  let concat ts ~dir = List.map ~f:(to_string ~dir) ts |> String.concat ~sep:" "

  let deps_only =
    List.filter_map ~f:(function
      | Dir _ | String _ -> None
      | Path p -> Some p)

  let strings = List.map ~f:(fun x -> String x)

  let paths = List.map ~f:(fun x -> Path x)

  let dirs = List.map ~f:(fun x -> Dir x)
end
