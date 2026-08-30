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

let append_component x y =
  (* Strip a trailing directory separator from [x] so we don't produce double
     slashes (e.g. "/root/" + "foo" -> "/root/foo"). We use [is_dir_sep] so
     that on Windows a trailing '\' is also removed, normalising the join to
     always use '/'. *)
  let x =
    let len = String.length x in
    if len > 0 && is_dir_sep x.[len - 1] then String.take x (len - 1) else x
  in
  String.append_with_char x ~sep:'/' y
;;

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
     | _ -> append_component x y)
;;

let relative_fname t fn = append_component t (Filename.to_string fn)
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

let posix_root_and_local t =
  match String.drop_prefix t ~prefix:"/" with
  | None -> None
  | Some t when String.starts_with ~prefix:"/" t -> None
  | Some "" -> Some ("/", Local.root)
  | Some t -> Some ("/", Local.of_string t)
;;

(* Relative paths on Windows are only meaningful within the same root. Roots
   can be the current drive, a drive letter, or a UNC server and share. Extended
   drive and UNC paths use the same structure after their [\\?\] prefix. *)
let windows_root_and_local t =
  let t =
    String.map t ~f:(function
      | '\\' -> '/'
      | c -> c)
  in
  let length = String.length t in
  let lowercase = String.lowercase t in
  let rec skip_separators pos =
    if pos < length && Char.equal t.[pos] '/' then skip_separators (pos + 1) else pos
  in
  let make ~root_end ~local_start =
    let root = String.take t root_end |> String.lowercase in
    let local_start = skip_separators local_start in
    let local = String.drop t local_start |> Local.of_string in
    Some (root, local)
  in
  let unc ~start =
    match String.index_from t start '/' with
    | None -> None
    | Some server_end ->
      let share_start = server_end + 1 in
      if server_end = start || share_start = length
      then None
      else (
        match String.index_from t share_start '/' with
        | None -> make ~root_end:length ~local_start:length
        | Some share_end ->
          if share_end = share_start
          then None
          else make ~root_end:share_end ~local_start:(share_end + 1))
  in
  let is_drive pos =
    pos + 2 < length
    && (match t.[pos] with
        | 'A' .. 'Z' | 'a' .. 'z' -> true
        | _ -> false)
    && Char.equal t.[pos + 1] ':'
    && Char.equal t.[pos + 2] '/'
  in
  if String.starts_with lowercase ~prefix:"//?/unc/"
  then unc ~start:8
  else if String.starts_with lowercase ~prefix:"//?/" && is_drive 4
  then make ~root_end:6 ~local_start:7
  else if
    String.starts_with lowercase ~prefix:"//?/"
    || String.starts_with lowercase ~prefix:"//./"
  then None
  else if String.starts_with t ~prefix:"//"
  then unc ~start:2
  else if length > 0 && Char.equal t.[0] '/'
  then make ~root_end:1 ~local_start:1
  else if is_drive 0
  then make ~root_end:2 ~local_start:3
  else None
;;

let root_and_local = if Sys.win32 then windows_root_and_local else posix_root_and_local

let reach t ~from =
  match root_and_local t, root_and_local from with
  | Some (root, t), Some (from_root, from) when String.equal root from_root ->
    Local.reach t ~from
  | _ -> to_string t
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

  let of_listing ~dir ~filenames = of_list_map filenames ~f:(relative_fname dir)
end
