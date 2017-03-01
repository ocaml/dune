open Import

let explode_path =
  let rec loop path acc =
    let dir  = Filename.dirname  path in
    let base = Filename.basename path in
    let acc = base :: acc in
    if dir = Filename.current_dir_name then
      acc
    else
      loop dir acc
  in
  fun path -> loop path []

module External = struct
  type t = string

  let to_string t = t
(*
  let rec cd_dot_dot t =
    match Unix.readlink t with
    | exception _ -> Filename.dirname t
    | t -> cd_dot_dot t

  let relative initial_t path =
    let rec loop t components =
      match components with
      | [] | ["." | ".."] ->
        die "invalid filename concatenation: %s / %s" initial_t path
      | [fn] -> Filename.concat t fn
      | "."  :: rest -> loop t rest
      | ".." :: rest -> loop (cd_dot_dot t) rest
      | comp :: rest -> loop (Filename.concat t comp) rest
    in
    loop initial_t (explode_path path)
*)

  let relative = Filename.concat
end

let is_root = function
  | "" -> true
  | _  -> false

module Local = struct
  (* either "" for root, either a '/' separated list of components other that ".", ".."
     and not containing '/'. *)
  type t = string

  let root = ""

  let to_string = function
    | "" -> "."
    | t  -> t

  let to_list =
    let rec loop t acc i j =
      if i = 0 then
        String.sub t ~pos:0 ~len:j :: acc
      else
        match t.[i - 1] with
        | '/' -> loop t (String.sub t ~pos:i ~len:(j - i) :: acc) (i - 1) (i - 1)
        | _   -> loop t acc (i - 1) j
    in
    function
    | "" -> []
    | t  ->
      let len = String.length t in
      loop t [] len len

  let parent = function
    | "" ->
      code_errorf "Path.Local.parent called on the root"
    | t ->
      match String.rindex_from t (String.length t - 1) '/' with
      | exception Not_found -> ""
      | i -> String.sub t ~pos:0 ~len:i

  let basename = function
    | "" ->
      code_errorf "Path.Local.basename called on the root"
    | t ->
      let len = String.length t in
      match String.rindex_from t (len - 1) '/' with
      | exception Not_found -> t
      | i -> String.sub t ~pos:(i + 1) ~len:(len - i - 1)

  let relative initial_t path =
    let rec loop t components =
      match components with
      | [] -> t
      | "." :: rest -> loop t rest
      | ".." :: rest ->
        begin match t with
        | "" ->
          die "path outside the workspace: %s from %s" path
            (to_string initial_t)
        | t -> loop (parent t) rest
        end
      | fn :: rest ->
        match t with
        | "" -> loop fn rest
        | _ -> loop (t ^ "/" ^ fn) rest
    in
    loop initial_t (explode_path path)

  let rec mkdir_p = function
    | "" -> ()
    | t ->
      try
        Unix.mkdir t 0o777
      with
      | Unix.Unix_error (EEXIST, _, _) -> ()
      | Unix.Unix_error (ENOENT, _, _) as e ->
        match parent t with
        | "" -> raise e
        | p ->
          mkdir_p p;
          Unix.mkdir t 0o777

  let ensure_parent_directory_exists = function
    | "" -> ()
    | t -> mkdir_p (parent t)

  let append a b =
    match a, b with
    | "", x | x, "" -> x
    | _ -> a ^ "/" ^ b

  let descendant t ~of_ =
    match of_ with
    | "" -> Some t
    | _ ->
      let of_len = String.length of_ in
      let t_len = String.length t in
      if (t_len = of_len && t = of_) ||
         (t_len >= of_len && t.[of_len] = '/' && String.is_prefix t ~prefix:of_) then
        Some (String.sub t ~pos:(of_len + 1) ~len:(t_len - of_len - 1))
      else
        None

  let reach t ~from =
    let rec loop t from =
      match t, from with
      | a :: t, b :: from when a = b ->
        loop t from
      | _ ->
        match List.fold_left from ~init:t ~f:(fun acc _ -> ".." :: acc) with
        | [] -> "."
        | l -> String.concat l ~sep:"/"
    in
    loop (to_list t) (to_list from)
end

type t = string
let compare = String.compare

module Set = String_set
module Map = String_map

module Kind = struct
  type t =
    | External of External.t
    | Local    of Local.t
end

let is_local t = is_root t || Filename.is_relative t

let kind t : Kind.t =
  if is_local t then
    Local t
  else
    External t

let to_string = function
  | "" -> "."
  | t  -> t

let sexp_of_t t = Sexp.Atom (to_string t)

let root = ""

let relative t fn =
  if fn = "" then
    t
  else
    match is_local t, is_local fn with
    | true, true  -> Local.relative t fn
    | _   , false -> fn
    | false, true -> External.relative t fn

let of_string t = relative "" t

let absolute =
  let initial_dir = Sys.getcwd () in
  fun fn ->
    if is_local fn then
      Filename.concat initial_dir fn
    else
      fn

let reach t ~from =
  match is_local t, is_local from with
  | false, _ -> t
  | true, false ->
    Sexp.code_error "Path.reach called with invalid combination"
      [ "t"   , sexp_of_t t
      ; "from", sexp_of_t from
      ]
  | true, true -> Local.reach t ~from

let descendant t ~of_ =
  if is_local t && is_local of_ then
    Local.descendant t ~of_
  else
    None

let append a b =
  assert (is_local b);
  if is_local a then
    Local.append a b
  else
    Filename.concat a b

let basename t =
  if is_local t then
    Local.basename t
  else
    Filename.basename t

let parent t =
  if is_local t then
    Local.parent t
  else
    Filename.dirname t

let build_prefix = "_build/"

let is_in_build_dir t =
  String.is_prefix t ~prefix:build_prefix

let extract_build_context t =
  if is_local t && String.is_prefix t ~prefix:build_prefix then
    let i = String.length build_prefix in
    match String.index_from t i '/' with
    | exception _ -> None
    | j ->
      Some
        (String.sub t ~pos:i ~len:(j - i),
         String.sub t ~pos:(j + 1) ~len:(String.length t - j - 1))
  else
    None

let extract_build_context_dir t =
  if is_local t && String.is_prefix t ~prefix:build_prefix then
    let i = String.length build_prefix in
    match String.index_from t i '/' with
    | exception _ -> None
    | j ->
      Some
        (String.sub t ~pos:0 ~len:j,
         String.sub t ~pos:(j + 1) ~len:(String.length t - j - 1))
  else
    None

let exists t = Sys.file_exists (to_string t)
let readdir t = Sys.readdir (to_string t) |> Array.to_list
let is_directory t = Sys.is_directory (to_string t)
let rmdir t = Unix.rmdir (to_string t)
let unlink t = Unix.unlink (to_string t)

let extend_basename t ~suffix = t ^ suffix
