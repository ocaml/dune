open Stdune

type t =
  | String of string
  | Path of Path.t

let string_of_path ~dir p = Path.reach ~from:dir p

let to_string t ~dir =
  match t with
  | String s -> s
  | Path p -> string_of_path ~dir p

let to_path ?error_loc t ~dir =
  match t with
  | String s -> Path.relative ?error_loc dir s
  | Path p -> p

module L = struct
  let to_strings t ~dir = List.map t ~f:(to_string ~dir)

  let concat ts ~dir =
    List.map ~f:(to_string ~dir) ts
    |> String.concat ~sep:" "

  let paths_only =
    List.filter_map ~f:(function
      | String _ -> None
      | Path p -> Some p)

  let strings = List.map ~f:(fun x -> String x)

  let paths = List.map ~f:(fun x -> Path x)
end
