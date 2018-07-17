open Stdune

type t =
  | String of string
  | Dir of Path.t
  | Path of Path.t

let compare x y =
  match x, y with
  | String x, String y -> String.compare x y
  | String _, _        -> Lt
  | _       , String _ -> Gt
  | Dir x   , Dir y    -> Path.compare x y
  | Dir _   , _        -> Lt
  | _       , Dir _    -> Gt
  | Path x  , Path y   -> Path.compare x y

let string_of_path ~dir p = Path.reach ~from:dir p

let to_string t ~dir =
  match t with
  | String s -> s
  | Dir p
  | Path p -> string_of_path ~dir p

let compare_as_string ~dir x y =
  match x, y with
  | String x, String y                 -> String.compare x y
  | (Dir x | Path x), (Dir y | Path y) -> Path.compare x y
  | (Dir _ | Path _), String y         -> String.compare (to_string ~dir x) y
  | String x, (Dir _ | Path _)         -> String.compare x (to_string ~dir y)

let to_path ?error_loc t ~dir =
  match t with
  | String s -> Path.relative ?error_loc dir s
  | Dir p
  | Path p -> p

module L = struct
  let to_strings t ~dir = List.map t ~f:(to_string ~dir)

  let concat ts ~dir =
    List.map ~f:(to_string ~dir) ts
    |> String.concat ~sep:" "

  let deps_only =
    List.filter_map ~f:(function
      | Dir _
      | String _ -> None
      | Path p -> Some p)

  let strings = List.map ~f:(fun x -> String x)

  let paths = List.map ~f:(fun x -> Path x)

  let dirs = List.map ~f:(fun x -> Dir x)
end
