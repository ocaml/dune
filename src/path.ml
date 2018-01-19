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

  let is_root = function
    | "" -> true
    | _  -> false

  let to_string = function
    | "" -> "."
    | t  -> t

  let compare = String.compare

  module Set = String_set

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

  let relative ?error_loc t path =
    let rec loop t components =
      match components with
      | [] -> Ok t
      | "." :: rest -> loop t rest
      | ".." :: rest ->
        begin match t with
        | "" -> Error ()
        | t -> loop (parent t) rest
        end
      | fn :: rest ->
        match t with
        | "" -> loop fn rest
        | _ -> loop (t ^ "/" ^ fn) rest
    in
    match loop t (explode_path path) with
    | Ok t -> t
    | Error () ->
       Loc.fail_opt error_loc "path outside the workspace: %s from %s" path
         (to_string t)

  let is_canonicalized =
    let rec before_slash s i =
      if i < 0 then
        false
      else
        match s.[i] with
        | '/' -> false
        | '.' -> before_dot_slash s (i - 1)
        | _   -> in_component     s (i - 1)
    and before_dot_slash s i =
      if i < 0 then
        false
      else
        match s.[i] with
        | '/' -> false
        | '.' -> before_dot_dot_slash s (i - 1)
        | _   -> in_component         s (i - 1)
    and before_dot_dot_slash s i =
      if i < 0 then
        false
      else
        match s.[i] with
        | '/' -> false
        | _   -> in_component s (i - 1)
    and in_component s i =
      if i < 0 then
        true
      else
        match s.[i] with
        | '/' -> before_slash s (i - 1)
        | _   -> in_component s (i - 1)
    in
    fun s ->
      let len = String.length s in
      if len = 0 then
        true
      else
        before_slash s (len - 1)

  let of_string ?error_loc s =
    if is_canonicalized s then
      s
    else
      relative "" s ?error_loc

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
      if t_len = of_len then
        Option.some_if (t = of_) t
      else if (t_len >= of_len && t.[of_len] = '/' && String.is_prefix t ~prefix:of_) then
        Some (String.sub t ~pos:(of_len + 1) ~len:(t_len - of_len - 1))
      else
        None

  let is_descendant t ~of_ =
    match of_ with
    | "" -> true
    | _ ->
      let of_len = String.length of_ in
      let t_len = String.length t in
      (t_len = of_len && t = of_) ||
      (t_len >= of_len && t.[of_len] = '/' && String.is_prefix t ~prefix:of_)

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

module Set = struct
  include String_set
  let sexp_of_t t = Sexp.To_sexp.(list string) (String_set.elements t)
  let of_string_set = map
end

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

let to_string_maybe_quoted t =
  maybe_quoted (to_string t)

let root = ""

let relative ?error_loc t fn =
  if fn = "" then
    t
  else
    match is_local t, is_local fn with
    | true, true  -> Local.relative t fn ?error_loc
    | _   , false -> fn
    | false, true -> External.relative t fn

let of_string ?error_loc s =
  match s with
  | "" -> ""
  | s  ->
    if Filename.is_relative s then
      Local.of_string s ?error_loc
    else
      s

let t sexp = of_string (Sexp.Of_sexp.string sexp) ~error_loc:(Sexp.Ast.loc sexp)
let sexp_of_t t = Sexp.Atom (to_string t)

let absolute fn =
  if is_local fn then
    Filename.concat initial_cwd fn
  else
    fn

let to_absolute_filename t =
  if is_local t then begin
    let root = !Clflags.workspace_root in
    assert (not (Filename.is_relative root));
    Filename.concat root (to_string t)
  end else
    t

let reach t ~from =
  match is_local t, is_local from with
  | false, _ -> t
  | true, false ->
    Sexp.code_error "Path.reach called with invalid combination"
      [ "t"   , sexp_of_t t
      ; "from", sexp_of_t from
      ]
  | true, true -> Local.reach t ~from

let reach_for_running t ~from =
  match is_local t, is_local from with
  | false, _ -> t
  | true, false ->
    Sexp.code_error "Path.reach_for_running called with invalid combination"
      [ "t"   , sexp_of_t t
      ; "from", sexp_of_t from
      ]
  | true, true ->
    let s = Local.reach t ~from in
    if String.is_prefix s ~prefix:"../" then
      s
    else
      "./" ^ s

let descendant t ~of_ =
  if is_local t && is_local of_ then
    Local.descendant t ~of_
  else
    None

let is_descendant t ~of_ =
  if is_local t && is_local of_ then
    Local.is_descendant t ~of_
  else
    false

let append a b =
  if not (is_local b) then
    Sexp.code_error "Path.append called with non-local second path"
      [ "a", sexp_of_t a
      ; "b", sexp_of_t b
      ];
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

let build_dir = "_build"

let is_in_build_dir t =
  String.is_prefix t ~prefix:build_prefix

let is_in_source_tree t = is_local t && not (is_in_build_dir t)

let is_alias_stamp_file t =
  String.is_prefix t ~prefix:"_build/.aliases/"

let extract_build_context t =
  if String.is_prefix t ~prefix:build_prefix then
    let i = String.length build_prefix in
    match String.index_from t i '/' with
    | exception _ ->
      Some
        (String.sub t ~pos:i ~len:(String.length t - i),
         "")
    | j ->
      Some
        (String.sub t ~pos:i ~len:(j - i),
         String.sub t ~pos:(j + 1) ~len:(String.length t - j - 1))
  else
    None

let extract_build_context_dir t =
  if String.is_prefix t ~prefix:build_prefix then
    let i = String.length build_prefix in
    match String.index_from t i '/' with
    | exception _ ->
      Some (t, "")
    | j ->
      Some
        (String.sub t ~pos:0 ~len:j,
         String.sub t ~pos:(j + 1) ~len:(String.length t - j - 1))
  else
    None

let drop_build_context t =
  Option.map (extract_build_context t) ~f:snd

let drop_build_context_exn t =
  match extract_build_context t with
  | None -> Sexp.code_error "Path.drop_build_context_exn" [ "t", sexp_of_t t ]
  | Some (_, t) -> t

let drop_optional_build_context t =
  match extract_build_context t with
  | None -> t
  | Some (_, t) -> t

let split_first_component t =
  if is_local t && not (is_root t)then
    match String.index t '/' with
    | None -> Some (t, root)
    | Some i ->
      Some
        (String.sub t ~pos:0 ~len:i,
         String.sub t ~pos:(i + 1) ~len:(String.length t - i - 1))
  else
    None

let explode t =
  if is_root t then
    Some []
  else if is_local t then
    Some (String.split t ~on:'/')
  else
    None

let explode_exn t =
  if is_root t then
    []
  else if is_local t then
    String.split t ~on:'/'
  else
    Sexp.code_error "Path.explode_exn" ["path", Atom t]

let exists t = Sys.file_exists (to_string t)
let readdir t = Sys.readdir (to_string t) |> Array.to_list
let is_directory t =
  try Sys.is_directory (to_string t)
  with Sys_error _ -> false
let rmdir t = Unix.rmdir (to_string t)
let unlink t = Unix.unlink (to_string t)
let unlink_no_err t = try Unix.unlink (to_string t) with _ -> ()

let extend_basename t ~suffix = t ^ suffix

let insert_after_build_dir_exn =
  let error a b =
    Sexp.code_error
      "Path.insert_after_build_dir_exn"
      [ "path"  , Atom a
      ; "insert", Atom b
      ]
  in
  fun a b ->
    if not (is_local a) || String.contains b '/' then error a b;
    match String.lsplit2 a ~on:'/' with
    | Some ("_build", rest) ->
      sprintf "_build/%s/%s" b rest
    | _ ->
      error a b

let rm_rf =
  let rec loop dir =
    Array.iter (Sys.readdir dir) ~f:(fun fn ->
      let fn = Filename.concat dir fn in
      match Unix.lstat fn with
      | { st_kind = S_DIR; _ } -> loop fn
      | _                      -> Unix.unlink fn);
    Unix.rmdir dir
  in
  fun t ->
    let fn = to_string t in
    match Unix.lstat fn with
    | exception Unix.Unix_error(ENOENT, _, _) -> ()
    | _ -> loop fn

let change_extension ~ext t =
  let t = try Filename.chop_extension t with Not_found -> t in
  t ^ ext

let extension = Filename.extension

let pp = Format.pp_print_string

let drop_prefix t ~prefix =
  let t = to_string t in
  let prefix =
    to_string (
      if String.is_suffix prefix ~suffix:"/" then
        prefix
      else
        prefix ^ "/") in
  String.drop_prefix t ~prefix
