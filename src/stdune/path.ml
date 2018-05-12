let is_dir_sep =
  match Sys.os_type with
  | "Win32" | "Cygwin" -> fun c -> c = '/' || c = '\\' || c = ':'
  | _ -> fun c -> c = '/'

let explode_path =
  let rec start acc path i =
    if i < 0 then
      acc
    else if is_dir_sep (String.unsafe_get path i) then
      start acc path (i - 1)
    else
      component acc path i (i - 1)
  and component acc path end_ i =
    if i < 0 then
      String.sub path ~pos:0 ~len:(end_ + 1)::acc
    else if is_dir_sep (String.unsafe_get path i) then
      start
        (String.sub path ~pos:(i + 1) ~len:(end_ - i)::acc)
        path
        (i - 1)
    else
      component acc path end_ (i - 1)
  in
  fun path ->
    if path = Filename.current_dir_name then
      [path]
    else
      match start [] path (String.length path - 1) with
      | "." :: xs -> xs
      | xs -> xs

module External = struct
  type t = string

  let compare = String.compare

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

  module Set = String.Set

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
      Exn.code_error "Path.Local.parent called on the root" []
    | t ->
      match String.rindex_from t (String.length t - 1) '/' with
      | exception Not_found -> ""
      | i -> String.sub t ~pos:0 ~len:i

  let basename = function
    | "" ->
      Exn.code_error "Path.Local.basename called on the root" []
    | t ->
      let len = String.length t in
      match String.rindex_from t (len - 1) '/' with
      | exception Not_found -> t
      | i -> String.sub t ~pos:(i + 1) ~len:(len - i - 1)

  let relative ?error_loc t path =
    if not (Filename.is_relative path) then (
      Exn.code_error "Local.relative: received absolute path"
        [ "t", Usexp.atom_or_quoted_string t
        ; "path", Usexp.atom_or_quoted_string path
        ]
    );
    let rec loop t components =
      match components with
      | [] -> Result.Ok t
      | "." :: rest -> loop t rest
      | ".." :: rest ->
        begin match t with
        | "" -> Result.Error ()
        | t -> loop (parent t) rest
        end
      | fn :: rest ->
        match t with
        | "" -> loop fn rest
        | _ -> loop (t ^ "/" ^ fn) rest
    in
    match loop t (explode_path path) with
    | Result.Ok t -> t
    | Error () ->
       Exn.fatalf ?loc:error_loc "path outside the workspace: %s from %s" path
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
      (t_len > of_len && t.[of_len] = '/' && String.is_prefix t ~prefix:of_)

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

module Kind = struct
  type t =
    | External of External.t
    | Local    of Local.t

  let to_absolute_filename t ~root =
    match t with
    | External s -> s
    | Local l -> Filename.concat root (Local.to_string l)

  let to_string = function
    | Local "" -> "."
    | Local t
    | External t -> t

  let of_string s =
    if s = "" || Filename.is_relative s then
      Local s
    else
      External s

  let relative ?error_loc t fn =
    match t with
    | Local t -> Local (Local.relative ?error_loc t fn)
    | External t -> External (External.relative t fn)
end

let build_dir_kind = Kind.Local "_build"

let drop_build_dir p =
  match build_dir_kind, Kind.of_string p with
  | Kind.External bd, Kind.External p ->
    String.drop_prefix ~prefix:bd p
  | Kind.Local of_, Kind.Local p ->
    let open Option.O in
    Local.descendant p ~of_ >>| fun p ->
    if p = of_ then "" else p
  | Kind.Local _, Kind.External _
  | Kind.External _, Kind.Local _ -> None

let in_build_path s =
  match build_dir_kind, Kind.of_string s with
  | Kind.Local p, Kind.Local s -> Local.is_descendant s ~of_:p
  | Kind.External _, Kind.External _
  | Kind.Local _, Kind.External _
  | Kind.External _, Kind.Local _ -> false

module T : sig
  type t = private
    | External of External.t
    | In_source_tree of Local.t
    | In_build_dir of Local.t

  val compare : t -> t -> Ordering.t

  val in_build_dir : Local.t -> t
  val in_source_tree : Local.t -> t
  val external_ : Local.t -> t
end = struct
  type t =
    | External of External.t
    | In_source_tree of Local.t
    | In_build_dir of Local.t

  let compare x y =
    match x, y with
    | External x, External y -> External.compare x y
    | External _, (In_source_tree _ | In_build_dir _) -> Ordering.Gt
    | (In_source_tree _ | In_build_dir _), External _  -> Ordering.Lt
    | In_source_tree x, In_source_tree y -> Local.compare x y
    | In_source_tree _, In_build_dir _ -> Ordering.Gt
    | In_build_dir _, In_source_tree _ -> Ordering.Lt
    | In_build_dir x, In_build_dir y -> Local.compare x y

  let in_build_dir s =
    if String.is_prefix s ~prefix:"_build" then (
      Exn.code_error "in_build_dir: path is in build dir"
        [ "s", Sexp.To_sexp.string s ]
    );
    In_build_dir s

  let in_source_tree s =
    if String.is_prefix s ~prefix:"_build" then (
      Exn.code_error "in_source_tree: path is in build dir"
        [ "s", Sexp.To_sexp.string s ]
    );
    if in_build_path s then (
      Exn.code_error "in_source_tree: path is in build dir"
        [ "s", Sexp.To_sexp.string s ]
    ) else if not (Filename.is_relative s) then (
      Exn.code_error "in_source_tree: absolute path"
        [ "s", Sexp.To_sexp.string s ]
    ) else if String.is_prefix s ~prefix:".aliases/" then (
      Exn.code_error "in_source_tree: alias exist only in build dir"
        ["s", Sexp.To_sexp.string s]
    );
    In_source_tree s
  let external_ e = External e
end

include T

let build_dir = in_build_dir ""

let is_root = function
  | In_source_tree "" -> true
  | In_source_tree _
  | In_build_dir _
  | External _  -> false

module Map = Map.Make(T)

let kind = function
  | In_build_dir p -> Kind.relative build_dir_kind p
  | In_source_tree s -> Kind.Local s
  | External s -> Kind.External s

let is_local_fn t = t = "" || Filename.is_relative t

let is_local = function
  | In_build_dir _
  | In_source_tree _ -> true
  | External _ -> false

let to_string t = Kind.to_string (kind t)

let to_string_maybe_quoted t =
  String.maybe_quoted (to_string t)

let root = in_source_tree ""

let relative ?error_loc t fn =
  if fn = "" then
    t
  else if not (Filename.is_relative fn) then
    external_ fn
  else
    match t with
    | In_source_tree "" ->
      begin match drop_build_dir fn with
      | None -> in_source_tree (Local.relative "" fn ?error_loc)
      | Some fn -> in_build_dir (Local.relative "" fn ?error_loc)
      end
    | In_source_tree s -> in_source_tree (Local.relative s fn ?error_loc)
    | In_build_dir s -> in_build_dir (Local.relative s fn ?error_loc)
    | External s -> external_ (External.relative s fn)

let of_string ?error_loc s =
  match s with
  | "" -> in_source_tree ""
  | s  ->
    begin match drop_build_dir s with
    | Some s -> in_build_dir (Local.of_string s ?error_loc)
    | None ->
      if Filename.is_relative s then
        in_source_tree (Local.of_string s ?error_loc)
      else
        external_ s
    end

let t sexp = of_string (Sexp.Of_sexp.string sexp) ~error_loc:(Sexp.Ast.loc sexp)
let sexp_of_t t = Sexp.atom_or_quoted_string (to_string t)

let initial_cwd = Sys.getcwd ()

let absolute fn =
  external_ (
    if is_local_fn fn then
      Filename.concat initial_cwd fn
    else
      fn
  )

let to_absolute_filename t ~root = Kind.to_absolute_filename (kind t) ~root

let reach t ~from =
  match kind t, kind from with
  | External t, _ -> t
  | Local _, External _ ->
    Exn.code_error "Path.reach called with invalid combination"
      [ "t"   , sexp_of_t t
      ; "from", sexp_of_t from
      ]
  | Local t, Local from -> Local.reach t ~from

let reach_for_running ?(from=root) t =
  match kind t, kind from with
  | External _, _ -> t
  | Local _, External _ ->
    Exn.code_error "Path.reach_for_running called with invalid combination"
      [ "t"   , sexp_of_t t
      ; "from", sexp_of_t from
      ]
  | Local t, Local from ->
    let s = Local.reach t ~from in
    in_source_tree (
      if String.is_prefix s ~prefix:"../" then
        s
      else
        "./" ^ s
    )

let descendant t ~of_ =
  match kind t, kind of_ with
  | Local t, Local of_ -> Option.map ~f:in_source_tree (Local.descendant t ~of_)
  | _, _ -> None

let is_descendant t ~of_ =
  match kind t, kind of_ with
  | Local t, Local of_ -> Local.is_descendant t ~of_
  | _, _ -> false

let append a b =
  match kind b with
  | External _ ->
    Exn.code_error "Path.append called with non-local second path"
      [ "a", sexp_of_t a
      ; "b", sexp_of_t b
      ]
  | Local b ->
    begin match a with
    | In_source_tree a -> in_source_tree (Local.append a b)
    | In_build_dir a -> in_build_dir (Local.append a b)
    | External a -> external_ (Filename.concat a b)
    end

let basename t =
  match kind t with
  | Local t -> Local.basename t
  | External t -> Filename.basename t

let parent = function
  | External s ->
    let parent = Filename.dirname s in
    if parent = s then
      None
    else
      Some (external_ parent)
  | In_source_tree "" | In_build_dir ""  -> None
  | In_source_tree l -> Some (in_source_tree (Local.parent l))
  | In_build_dir l -> Some (in_build_dir (Local.parent l))

let parent_exn t =
  match parent t with
  | Some p -> p
  | None -> Exn.code_error "Path.parent:exn t is root"
              ["t", sexp_of_t t]

let is_in_build_dir = function
  | In_build_dir "" -> false
  | In_build_dir _ -> true
  | In_source_tree _
  | External _ -> false

let is_in_source_tree = function
  | In_source_tree _ -> true
  | In_build_dir _
  | External _ -> false

let is_alias_stamp_file = function
  | In_build_dir s -> String.is_prefix s ~prefix:".aliases/"
  | In_source_tree _
  | External _ -> false

let extract_build_context = function
  | In_source_tree _
  | External _ -> None
  | In_build_dir "" -> None
  | In_build_dir t ->
    begin match String.index t '/' with
    | None ->
      Some (String.sub t ~pos:0 ~len:(String.length t), in_source_tree "")
    | Some j ->
      Some
        (String.sub t ~pos:0 ~len:j,
         in_source_tree (String.sub t ~pos:(j + 1) ~len:(String.length t - j - 1)))
    end

let extract_build_context_dir = function
  | In_source_tree _
  | External _ -> None
  | In_build_dir t ->
    begin match String.index t '/' with
    | None -> Some (in_build_dir t, in_source_tree "")
    | Some j ->
      Some
        (in_build_dir (String.sub t ~pos:0 ~len:j),
         in_source_tree
           (String.sub t ~pos:(j + 1) ~len:(String.length t - j - 1)))
    end

let drop_build_context t =
  Option.map (extract_build_context t) ~f:snd

let drop_build_context_exn t =
  match extract_build_context t with
  | None -> Exn.code_error "Path.drop_build_context_exn" [ "t", sexp_of_t t ]
  | Some (_, t) -> t

let drop_optional_build_context t =
  match extract_build_context t with
  | None -> t
  | Some (_, t) -> t

let split_first_component t =
  match kind t, is_root t with
  | Local t, false ->
    begin match String.index t '/' with
    | None -> Some (t, root)
    | Some i ->
      Some
        (String.sub t ~pos:0 ~len:i,
         in_source_tree (
           String.sub t ~pos:(i + 1) ~len:(String.length t - i - 1)))
    end
  | _, _ -> None

let explode t =
  match kind t with
  | Local "" -> Some []
  | Local s -> Some (String.split s ~on:'/')
  | External _ -> None

let explode_exn t =
  match explode t with
  | Some s -> s
  | None -> Exn.code_error "Path.explode_exn"
              ["path", sexp_of_t t]

let exists t =
  try Sys.file_exists (to_string t)
  with Sys_error _ -> false
let readdir t = Sys.readdir (to_string t) |> Array.to_list
let is_directory t =
  try Sys.is_directory (to_string t)
  with Sys_error _ -> false
let rmdir t = Unix.rmdir (to_string t)
let win32_unlink fn =
  try
    Unix.unlink fn
  with Unix.Unix_error (Unix.EACCES, _, _) as e ->
    (* Try removing the read-only attribute *)
    try
      Unix.chmod fn 0o666;
      Unix.unlink fn
    with _ ->
      raise e
let unlink_operation =
  if Sys.win32 then
    win32_unlink
  else
    Unix.unlink
let unlink t =
  unlink_operation (to_string t)
let unlink_no_err t = try unlink t with _ -> ()

let build_dir_exists () = is_directory build_dir

let ensure_build_dir_exists () =
  match kind build_dir with
  | Local p -> Local.mkdir_p p
  | External _ -> failwith ""

let map_s t ~f =
  match t with
  | In_source_tree t -> in_source_tree (f t)
  | In_build_dir t -> in_build_dir (f t)
  | External t -> external_ (f t)

let extend_basename t ~suffix = map_s t ~f:(fun t -> t ^ suffix)

let insert_after_build_dir_exn =
  let error a b =
    Exn.code_error
      "Path.insert_after_build_dir_exn"
      [ "path"  , sexp_of_t a
      ; "insert", Sexp.unsafe_atom_of_string b
      ]
  in
  fun a b ->
    match a with
    | In_build_dir a -> in_build_dir (Local.relative b a)
    | In_source_tree _
    | External _ -> error a b

let rm_rf =
  let rec loop dir =
    Array.iter (Sys.readdir dir) ~f:(fun fn ->
      let fn = Filename.concat dir fn in
      match Unix.lstat fn with
      | { st_kind = S_DIR; _ } -> loop fn
      | _                      -> unlink_operation fn);
    Unix.rmdir dir
  in
  fun t ->
    if not (is_local t) then (
      Exn.code_error "Path.rm_rf called on external dir"
        ["t", sexp_of_t t]
    );
    let fn = to_string t in
    match Unix.lstat fn with
    | exception Unix.Unix_error(ENOENT, _, _) -> ()
    | _ -> loop fn

let change_extension ~ext =
  map_s ~f:(fun t ->
    let t = try Filename.chop_extension t with Not_found -> t in
    t ^ ext
  )

let extension t = Filename.extension (to_string t)

let pp ppf t = Format.pp_print_string ppf (to_string t)

let pp_debug ppf = function
  | In_source_tree s -> Format.fprintf ppf "(In_source_tree %S)" s
  | In_build_dir s -> Format.fprintf ppf "(In_build_dir %S)" s
  | External s -> Format.fprintf ppf "(External %S)" s

module Set = struct
  include Set.Make(T)
  let sexp_of_t t = Sexp.To_sexp.(list sexp_of_t) (to_list t)
  let of_string_set ss ~f =
    String.Set.to_list ss
    |> List.map ~f
    |> of_list
end
