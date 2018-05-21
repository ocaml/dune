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

module External : sig
  type t

  val compare : t -> t -> Ordering.t
  val t : t Sexp.Of_sexp.t
  val sexp_of_t : t Sexp.To_sexp.t
  val to_string : t -> string
  val of_string : string -> t
  val relative : t -> string -> t
  val mkdir_p : t -> unit
  val basename : t -> string
  val parent : t -> t
  val initial_cwd : t
  val cwd : unit -> t
  val extend_basename : t -> suffix:string -> t
end = struct
  type t = string

  let extend_basename t ~suffix = t ^ suffix

  let compare = String.compare

  let to_string t = t

  let of_string t =
    if Filename.is_relative t then
      Exn.code_error "Path.External.of_string: relative path given"
        [ "t", Sexp.To_sexp.string t ];
    t

  let sexp_of_t t = Sexp.To_sexp.string (to_string t)
  let t sexp =
    let t = Sexp.Of_sexp.string sexp in
    if Filename.is_relative t then
      Sexp.Of_sexp.of_sexp_error sexp "Absolute path expected";
    t

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

  let rec mkdir_p t =
    let p = Filename.dirname t in
    if p <> t then
      try
        Unix.mkdir t 0o777
      with
      | Unix.Unix_error (EEXIST, _, _) -> ()
      | Unix.Unix_error (ENOENT, _, _) ->
        mkdir_p p;
        Unix.mkdir t 0o777

  let basename = Filename.basename
  let parent = Filename.dirname

  let cwd = Sys.getcwd
  let initial_cwd = cwd ()
end

module Local : sig
  type t

  val t : t Sexp.Of_sexp.t
  val sexp_of_t : t Sexp.To_sexp.t
  val root : t
  val is_root : t -> bool
  val compare : t -> t -> Ordering.t
  val of_string : ?error_loc:Usexp.Loc.t -> string -> t
  val to_string : t -> string
  val relative : ?error_loc:Usexp.Loc.t -> t -> string -> t
  val append : t -> t -> t
  val parent : t -> t
  val mkdir_p : t -> unit
  val descendant : t -> of_:t -> t option
  val is_descendant : t -> of_:t -> bool
  val reach : t -> from:t -> t
  val basename : t -> string
  val ensure_parent_directory_exists : t -> unit
  val extend_basename : t -> suffix:string -> t
  module Set : Set.S with type elt = t
end = struct
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

  let sexp_of_t t = Sexp.To_sexp.string (to_string t)
  let t sexp =
    of_string (Sexp.Of_sexp.string sexp)
      ~error_loc:(Sexp.Ast.loc sexp)

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

  let extend_basename t ~suffix = t ^ suffix
end

let (_abs_root, set_root) =
  let root_dir = ref None in
  let set_root new_root =
    match !root_dir with
    | None -> root_dir := Some new_root
    | Some root_dir ->
      Exn.code_error "set_root: cannot set root_dir more than once"
        [ "root_dir", External.sexp_of_t root_dir
        ; "new_root_dir", External.sexp_of_t new_root
        ]
  in
  let abs_root () =
    match !root_dir with
    | None ->
      Exn.code_error "root_dir: cannot use root dir before it's set" []
    | Some root_dir -> root_dir in
  (abs_root, set_root)

module Kind = struct
  type t =
    | External of External.t
    | Local    of Local.t

  let to_absolute_filename t ~root =
    match t with
    | External s -> External.to_string s
    | Local l -> External.to_string (External.relative root (Local.to_string l))

  let to_string = function
    | Local t -> Local.to_string t
    | External t -> External.to_string t

  let sexp_of_t t = Sexp.atom_or_quoted_string (to_string t)

  let of_string s =
    if s = "" || Filename.is_relative s then
      Local (Local.of_string s)
    else
      External (External.of_string s)

  let relative ?error_loc t fn =
    match t with
    | Local t -> Local (Local.relative ?error_loc t fn)
    | External t -> External (External.relative t fn)

  let mkdir_p = function
    | Local t -> Local.mkdir_p t
    | External t -> External.mkdir_p t

  let append_local x y =
    match x with
    | Local x -> Local (Local.append x y)
    | External x -> External (External.relative x (Local.to_string y))

end

let (build_dir_kind, set_build_dir) =
  let build_dir = ref None in
  let set_build_dir new_build_dir =
    match !build_dir with
    | None -> build_dir := Some new_build_dir
    | Some build_dir ->
      Exn.code_error "set_build_dir: cannot set build_dir more than once"
        [ "build_dir", Kind.sexp_of_t build_dir
        ; "New_build_dir", Kind.sexp_of_t new_build_dir ]
  in
  let build_dir () =
    match !build_dir with
    | None ->
      Exn.code_error "build_dir: cannot use build dir before it's set" []
    | Some build_dir -> build_dir in
  (build_dir, set_build_dir)

let as_relative_to_build_dir p =
  if not (Filename.is_relative p) then
    None
  else
    match build_dir_kind () with
    | External _ -> None
    | Local of_ ->
      Local.descendant ~of_ (Local.of_string p)
      |> Option.map ~f:(fun p -> if p = of_ then Local.root else p)

module T : sig
  type t = private
    | External of External.t
    | In_source_tree of Local.t
    | In_build_dir of Local.t

  val compare : t -> t -> Ordering.t

  val in_build_dir : Local.t -> t
  val in_source_tree : Local.t -> t
  val external_ : External.t -> t
end = struct
  type t =
    | External of External.t
    | In_source_tree of Local.t
    | In_build_dir of Local.t

  let compare x y =
    match x, y with
    | External x      , External y       -> External.compare x y
    | External _      , _                -> Lt
    | _               , External _       -> Gt
    | In_source_tree x, In_source_tree y -> Local.compare x y
    | In_source_tree _, _                -> Lt
    | _               , In_source_tree _ -> Gt
    | In_build_dir x  , In_build_dir y   -> Local.compare x y

  let in_build_dir s = In_build_dir s
  let in_source_tree s = In_source_tree s
  let external_ e = External e
end

include T

let build_dir = in_build_dir Local.root

let is_root = function
  | In_source_tree s -> Local.is_root s
  | In_build_dir _
  | External _  -> false

module Map = Map.Make(T)

let kind = function
  | In_build_dir p -> Kind.relative (build_dir_kind ()) (Local.to_string p)
  | In_source_tree s -> Kind.Local s
  | External s -> Kind.External s

let is_local_fn t = t = "" || Filename.is_relative t

let is_managed = function
  | In_build_dir _
  | In_source_tree _ -> true
  | External _ -> false

let to_string t = Kind.to_string (kind t)

let to_string_maybe_quoted t =
  String.maybe_quoted (to_string t)

let root = in_source_tree Local.root

let relative ?error_loc t fn =
  if fn = "" then
    t
  else if not (Filename.is_relative fn) then
    match as_relative_to_build_dir fn with
    | None -> external_ (External.of_string fn)
    | Some fn -> in_build_dir fn
  else
    match t with
    | In_source_tree p when Local.is_root p ->
      begin match as_relative_to_build_dir fn with
      | None -> in_source_tree (Local.relative Local.root fn ?error_loc)
      | Some fn -> in_build_dir fn
      end
    | In_source_tree s -> in_source_tree (Local.relative s fn ?error_loc)
    | In_build_dir s -> in_build_dir (Local.relative s fn ?error_loc)
    | External s -> external_ (External.relative s fn)

let of_string ?error_loc s =
  match s with
  | "" -> in_source_tree Local.root
  | s  ->
    begin match as_relative_to_build_dir s with
    | Some s -> in_build_dir s
    | None ->
      if Filename.is_relative s then
        in_source_tree (Local.of_string s ?error_loc)
      else
        external_ (External.of_string s)
    end

let t = function
  (* the first 2 cases are necessary for old build dirs *)
  | Sexp.Ast.Atom (_, A s)
  | Quoted_string (_, s) -> of_string s
  | s ->
    let open Sexp.Of_sexp in
    sum
      [ cstr "In_build_dir" (Local.t @> nil) in_build_dir
      ; cstr "In_source_tree" (Local.t @> nil) in_source_tree
      ; cstr "External" (External.t @> nil) external_
      ] s

let sexp_of_t t =
  let constr f x y = Sexp.To_sexp.(pair string f) (x, y) in
  match t with
  | In_build_dir s -> constr Local.sexp_of_t "In_build_dir" s
  | In_source_tree s -> constr Local.sexp_of_t "In_source_tree" s
  | External s -> constr External.sexp_of_t "External" s

let absolute fn =
  external_ (
    if is_local_fn fn then
      External.relative External.initial_cwd fn
    else
      External.of_string fn
  )

let to_absolute_filename t ~root = Kind.to_absolute_filename (kind t) ~root

let reach t ~from =
  match kind t, kind from with
  | External t, _ -> External.to_string t
  | (Local _) as l, External _ ->
    Kind.to_absolute_filename l ~root:External.initial_cwd
  | Local t, Local from -> Local.to_string (Local.reach t ~from)

let reach_for_running ?(from=root) t =
  match kind t, kind from with
  | External t, _ -> External.to_string t
  | Local _, External _ ->
    Exn.code_error "Path.reach_for_running called with invalid combination"
      [ "t"   , sexp_of_t t
      ; "from", sexp_of_t from
      ]
  | Local t, Local from ->
    let reach = Local.reach t ~from in
    let s = Local.to_string reach in
    if String.is_prefix s ~prefix:"../" then
      s
    else
      "./" ^ s

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
    | External a -> external_ (External.relative a (Local.to_string b))
    end

let basename t =
  match kind t with
  | Local t -> Local.basename t
  | External t -> External.basename t

let parent = function
  | External s ->
    let parent = External.parent s in
    if parent = s then
      None
    else
      Some (external_ parent)
  | In_source_tree p | In_build_dir p when Local.is_root p  -> None
  | In_source_tree l -> Some (in_source_tree (Local.parent l))
  | In_build_dir l -> Some (in_build_dir (Local.parent l))

let parent_exn t =
  match parent t with
  | Some p -> p
  | None -> Exn.code_error "Path.parent:exn t is root"
              ["t", sexp_of_t t]

let is_in_build_dir = function
  | In_build_dir p -> not (Local.is_root p)
  | In_source_tree _
  | External _ -> false

let is_in_source_tree = function
  | In_source_tree _ -> true
  | In_build_dir _
  | External _ -> false

let is_alias_stamp_file = function
  | In_build_dir s -> String.is_prefix (Local.to_string s) ~prefix:".aliases/"
  | In_source_tree _
  | External _ -> false

let extract_build_context = function
  | In_source_tree _
  | External _ -> None
  | In_build_dir p when Local.is_root p -> None
  | In_build_dir t ->
    let t = Local.to_string t in
    begin match String.index t '/' with
    | None ->
      Some ( String.sub t ~pos:0 ~len:(String.length t)
           , in_source_tree Local.root )
    | Some j ->
      Some
        ( String.sub t ~pos:0 ~len:j
        , String.sub t ~pos:(j + 1) ~len:(String.length t - j - 1)
          |> Local.of_string
          |> in_source_tree )
    end

let extract_build_context_dir = function
  | In_source_tree _
  | External _ -> None
  | In_build_dir t ->
    let t_str = Local.to_string t in
    begin match String.index t_str '/' with
    | None -> Some (in_build_dir t, in_source_tree Local.root)
    | Some j ->
      Some
        ( in_build_dir (Local.of_string (String.sub t_str ~pos:0 ~len:j))
        , (String.sub t_str ~pos:(j + 1) ~len:(String.length t_str - j - 1))
          |> Local.of_string
          |> in_source_tree
        )
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
    let t = Local.to_string t in
    begin match String.index t '/' with
    | None -> Some (t, root)
    | Some i ->
      Some
        ( String.sub t ~pos:0 ~len:i
        , String.sub t ~pos:(i + 1) ~len:(String.length t - i - 1)
          |> Local.of_string
          |> in_source_tree )
    end
  | _, _ -> None

let explode t =
  match kind t with
  | Local p when Local.is_root p -> Some []
  | Local s -> Some (String.split (Local.to_string s) ~on:'/')
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
  | External p ->
    let p = External.to_string p in
    try
      Unix.mkdir p 0o777
    with
    | Unix.Unix_error (EEXIST, _, _) -> ()
    | Unix.Unix_error (ENOENT, _, _) ->
      Exn.fatalf "Cannot create external build directory %s. \
                  Make sure that the parent dir %s exists."
        p (Filename.dirname p)

let extend_basename t ~suffix =
  match t with
  | In_source_tree t -> in_source_tree (Local.extend_basename t ~suffix)
  | In_build_dir t -> in_build_dir (Local.extend_basename t ~suffix)
  | External t -> external_ (External.extend_basename t ~suffix)

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
    | In_build_dir a -> in_build_dir (Local.append (Local.of_string b) a)
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
    if not (is_managed t) then (
      Exn.code_error "Path.rm_rf called on external dir"
        ["t", sexp_of_t t]
    );
    let fn = to_string t in
    match Unix.lstat fn with
    | exception Unix.Unix_error(ENOENT, _, _) -> ()
    | _ -> loop fn

let mkdir_p = function
  | External s ->
    Exn.code_error "Path.mkdir_p cannot create external path"
      ["s", External.sexp_of_t s]
  | In_source_tree s ->
    Exn.code_error "Path.mkdir_p cannot dir in source"
      ["s", Local.sexp_of_t s]
  | In_build_dir k -> Kind.mkdir_p (Kind.append_local (build_dir_kind ()) k)

let extension t = Filename.extension (to_string t)

let pp ppf t = Format.pp_print_string ppf (to_string t)

let pp_debug ppf = function
  | In_source_tree s ->
    Format.fprintf ppf "(In_source_tree %S)" (Local.to_string s)
  | In_build_dir s ->
    Format.fprintf ppf "(In_build_dir %S)" (Local.to_string s)
  | External s -> Format.fprintf ppf "(External %S)" (External.to_string s)

module Set = struct
  include Set.Make(T)
  let sexp_of_t t = Sexp.To_sexp.(list sexp_of_t) (to_list t)
  let of_string_set ss ~f =
    String.Set.to_list ss
    |> List.map ~f
    |> of_list
end

let in_source s = in_source_tree (Local.of_string s)
