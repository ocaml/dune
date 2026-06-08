open Path0
module Table = String.Table

type t = string

let to_string t = t
let repr = Repr.view Repr.string ~to_:to_string
let equal = String.equal
let hash = String.hash
let compare = String.compare
let extend_basename t ~suffix = t ^ Filename.to_string suffix

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
  | _ ->
    let y =
      if String.length y >= 2 && y.[0] = '.' && is_dir_sep y.[1]
      then String.drop y 2
      else y
    in
    (match y with
     | "" | "." -> x
     | _ ->
       (* Strip a trailing directory separator from [x] so we don't produce
          double slashes (e.g. "/root/" + "foo" -> "/root/foo"). We use
          [is_dir_sep] so that on Windows a trailing '\' is also removed,
          normalising the join to always use '/'. *)
       let x =
         let len = String.length x in
         if len > 0 && is_dir_sep x.[len - 1] then String.take x (len - 1) else x
       in
       String.concat ~sep:"/" [ x; y ])
;;

let relative_fname t fn = relative t (Filename.to_string fn)
let append_local t local = relative t (Local.to_string local)
let root = of_string "/"
let is_root = equal root

let basename t =
  if is_root t
  then Code_error.raise "Path.External.basename called on the root" []
  else Filename.basename t |> Filename.of_string_exn
;;

let basename_opt = basename_opt ~is_root ~basename
let parent t = if is_root t then None else Some (Filename.dirname t)

let parent_exn t =
  match parent t with
  | None -> Code_error.raise "Path.External.parent_exn called on a root path" []
  | Some p -> p
;;

let extension t = Stdlib.Filename.extension t |> Filename.Extension.Or_empty.of_string_exn

let split_extension t =
  let ext = extension t in
  Filename.Extension.Or_empty.drop_suffix t ext, ext
;;

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
let as_local t = "." ^ t

let is_slash = function
  | '/' -> true
  | '\\' when Sys.win32 -> true
  | _ -> false
;;

let skip_slashes t pos =
  let len = String.length t in
  let rec loop pos = if pos < len && is_slash t.[pos] then loop (pos + 1) else pos in
  loop pos
;;

let find_slash t pos =
  let len = String.length t in
  let rec loop pos =
    if pos = len then None else if is_slash t.[pos] then Some pos else loop (pos + 1)
  in
  loop pos
;;

let local_part t pos =
  let len = String.length t in
  let pos = skip_slashes t pos in
  if pos = len then Local.root else Local.of_string (String.drop t pos)
;;

let unc_root t =
  match find_slash t 2 with
  | None -> t, Local.root
  | Some server_end ->
    let share_start = skip_slashes t server_end in
    (match find_slash t share_start with
     | None -> t, Local.root
     | Some share_end -> String.take t share_end, local_part t share_end)
;;

let split_root t =
  let len = String.length t in
  if Sys.win32 && len >= 2 && Char.equal t.[1] ':'
  then String.lowercase (String.take t 2), local_part t 2
  else if Sys.win32 && len >= 2 && is_slash t.[0] && is_slash t.[1]
  then (
    let root, local = unc_root t in
    String.lowercase root, local)
  else "/", local_part t 0
;;

let reach t ~from =
  let root, local = split_root t in
  let from_root, from_local = split_root from in
  if String.equal root from_root then Local.reach local ~from:from_local else to_string t
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
  is_root a || String.starts_with ~prefix:(to_string a ^ "/") (to_string b)
;;

module Map = String.Map

module Set = struct
  include String.Set

  let of_listing ~dir ~filenames =
    of_list_map filenames ~f:(fun f -> relative dir (Filename.to_string f))
  ;;
end
