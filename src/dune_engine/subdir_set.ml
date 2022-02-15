open! Stdune

type t =
  | All
  | These of String.Set.t

let to_dir_set = function
  | All -> Dir_set.universal
  | These s ->
    String.Set.fold s ~init:Dir_set.empty ~f:(fun path acc ->
        let path = Path.Local.of_string path in
        Dir_set.union acc (Dir_set.singleton path))

let of_dir_set d =
  match Dir_set.toplevel_subdirs d with
  | Infinite -> All
  | Finite s -> These s

let of_list l = These (String.Set.of_list l)

let empty = These String.Set.empty

let is_empty = function
  | All -> false
  | These set -> String.Set.is_empty set

let mem t dir =
  match t with
  | All -> true
  | These t -> String.Set.mem t dir

let union a b =
  match (a, b) with
  | All, _ | _, All -> All
  | These a, These b -> These (String.Set.union a b)

let inter_set = function
  | All -> Fun.id
  | These t -> String.Set.inter t

let union_all = List.fold_left ~init:empty ~f:union
