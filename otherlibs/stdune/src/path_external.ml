open Path0
module Table = String.Table

type t = string

let to_string t = t
let equal = String.equal
let hash = String.hash
let compare = String.compare
let extend_basename t ~suffix = t ^ suffix

let of_string t =
  if Filename.is_relative t
  then Code_error.raise "Path.External.of_string: relative path given" [ "t", String t ];
  t
;;

let parse_string_exn ~loc t =
  if Filename.is_relative t
  then User_error.raise ~loc [ Pp.textf "path %s is not absolute" t ];
  t
;;

let to_dyn t = Dyn.variant "External" [ Dyn.string t ]

let relative x y =
  match y with
  | "." -> x
  | _ -> Filename.concat x y
;;

let append_local t local = relative t (Local.to_string local)
let basename t = Filename.basename t
let root = of_string "/"
let is_root = equal root
let basename_opt = basename_opt ~is_root ~basename
let parent t = if is_root t then None else Some (Filename.dirname t)

let parent_exn t =
  match parent t with
  | None -> Code_error.raise "Path.External.parent_exn called on a root path" []
  | Some p -> p
;;

let extension t = Filename.extension t
let split_extension t = Filename.split_extension t

let set_extension t ~ext =
  let base, _ = split_extension t in
  base ^ Filename.Extension.to_string ext
;;

let map_extension t ~f =
  let base, ext = split_extension t in
  base ^ Filename.Extension.Or_empty.to_string (f ext)
;;

let cwd () = Sys.getcwd ()
let initial_cwd = Fpath.initial_cwd

let as_local t =
  let s = t in
  "." ^ s
;;

let of_filename_relative_to_initial_cwd fn =
  if Filename.is_relative fn then relative initial_cwd fn else of_string fn
;;

include (
  Comparator.Operators (struct
    type nonrec t = t

    let compare = compare
  end) :
    Comparator.OPS with type t := t)

let to_string_maybe_quoted t = String.maybe_quoted (to_string t)

let is_descendant b ~of_:a =
  if is_root a then true else String.starts_with ~prefix:(to_string a ^ "/") (to_string b)
;;

module Map = String.Map

module Set = struct
  include String.Set

  let of_listing ~dir ~filenames = of_list_map filenames ~f:(fun f -> relative dir f)
end
