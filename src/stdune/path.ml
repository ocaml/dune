let is_dir_sep =
  if Sys.win32 || Sys.cygwin then
    fun c -> c = '/' || c = '\\' || c = ':'
  else
    fun c -> c = '/'

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
      String.take path (end_ + 1) :: acc
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
  include Path_intf.Local
  val compare_val : t -> t -> Ordering.t
  val relative : t -> string -> t
  val mkdir_p : t -> unit
  val initial_cwd : t
  val cwd : unit -> t
  val as_local : t -> string
end = struct
  module T = Interned.No_interning(struct
      let initial_size = 512
      let resize_policy = Interned.Greedy
      let order = Interned.Natural
    end)()

  module Table = Hashtbl.Make(T)

  type t = T.t

  let to_string = T.to_string
  let make = T.make
  let equal = T.equal
  let hash = T.hash
  let pp = T.pp
  let compare = T.compare

  let compare_val x y = String.compare (to_string x) (to_string y)

  let as_string x ~f =
    to_string x
    |> f
    |> make

  let extend_basename t ~suffix = as_string t ~f:(fun t -> t ^ suffix)

  let is_suffix t ~suffix = String.is_suffix (to_string t) ~suffix

  let of_string t =
    if Filename.is_relative t then
      Exn.code_error "Path.External.of_string: relative path given"
        [ "t", Sexp.Encoder.string t ];
    make t

  let parse_string_exn ~loc t =
    if Filename.is_relative t then
      Exn.fatalf ~loc "path %s is not absolute" t;
    make t

  let to_sexp t = Sexp.Encoder.string (to_string t)
  let to_dyn t = Dyn.String (to_string t)

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

  let relative x y =
    match y with
    | "." -> x
    | _   -> make (Filename.concat (to_string x) y)

  let rec mkdir_p t =
    let t_s = to_string t in
    let p_s = Filename.dirname t_s in
    let p = make p_s in
    if p <> t then
      try
        Unix.mkdir t_s 0o777
      with
      | Unix.Unix_error (EEXIST, _, _) -> ()
      | Unix.Unix_error (ENOENT, _, _) ->
        mkdir_p p;
        Unix.mkdir t_s 0o777

  let basename t = Filename.basename (to_string t)

  let root = of_string "/"

  let is_root = equal root

  let parent t =
    if is_root t then
      None
    else
      Some (as_string t ~f:Filename.dirname)

  let extension t = Filename.extension (to_string t)
  let split_extension t =
    let s, ext = Filename.split_extension (to_string t) in
    (make s, ext)

  let set_extension t ~ext =
    let (base, _) = split_extension t in
    (to_string base) ^ ext
    |> make

  let cwd () = make (Sys.getcwd ())
  let initial_cwd = cwd ()

  let as_local t =
    let s = to_string t in
    "." ^ s

  include (
    Comparable.Operators(struct
      type nonrec t = t
      let compare = compare_val
    end)
    : Comparable.OPS with type t := t
  )

  let to_string_maybe_quoted t =
    String.maybe_quoted (to_string t)

  let parent_exn t =
    match parent t with
    | None -> Exn.code_error "Path.External.parent_exn called on a root path" []
    | Some p -> p

  module Set = struct
    include T.Set
    let of_listing ~dir ~filenames =
      of_list (List.map filenames ~f:(fun f -> relative dir f))
  end

  module Map = T.Map
end

module Relative : sig
  include Path_intf.S

  val root : t
  val is_root : t -> bool
  val relative : t -> string -> t
  val append : t -> t -> t

  val split_first_component : t -> (string * t) option
  val explode : t -> string list

  module L : sig
    val relative : t -> string list -> t
  end
end = struct
  module T = Interned.No_interning(struct
      let initial_size = 512
      let resize_policy = Interned.Greedy
      let order = Interned.Natural
    end)()

  type t = T.t

  let to_string = T.to_string

  let root = T.make "."

  let compare = T.compare

  let is_root t = Ordering.is_eq (compare t root)

  let make t =
    if t = "" then
      root
    else if Filename.is_relative t then
      T.make t
    else
      Exn.code_error "Relative.make: not relative path"
        ["t", Sexp.Encoder.string t]
  let hash = T.hash

  let pp ppf s = Format.pp_print_string ppf (to_string s)

  let compare_val x y = String.compare (to_string x) (to_string y)

  let is_suffix t ~suffix = String.is_suffix (to_string t) ~suffix

  let to_sexp t = Sexp.Encoder.string (to_string t)

  let basename t =
    if is_root t then
      Exn.code_error "Path.Local.basename called on the root" []
    else
      Filename.basename (to_string t)

  let to_dyn t = Dyn.String (to_string t)

  let relative t s =
    make (Filename.concat (to_string t) s)

  module L = struct
    let relative t components =
      List.fold_left ~init:t ~f:relative components
  end

  let relative t path =
    relative t path

  let parse_string_exn ~loc s =
    ignore loc;
    match s with
    | "" | "." -> root
    | _ -> make s
  let of_string s = parse_string_exn ~loc:Loc0.none s

  let append a b =
    match is_root a, is_root b with
    | true, _ -> b
    | _, true -> a
    | _, _ -> make ((to_string a) ^ "/" ^ (to_string b))

  let extend_basename t ~suffix = make (to_string t ^ suffix)

  let extension t = Filename.extension (to_string t)
  let split_extension t =
    let s, ext = Filename.split_extension (to_string t) in
    (make s, ext)

  let set_extension t ~ext =
    let (base, _) = split_extension t in
    (to_string base) ^ ext
    |> make

  include (
    Comparable.Operators(struct
      type nonrec t = t
      let compare = compare_val
    end)
    : Comparable.OPS with type t := t
  )

  let split_first_component t =
    if is_root t then None
    else
      let t = (to_string t) in
      begin match String.lsplit2 t ~on:'/' with
      | None -> Some (t, root)
      | Some (before, after) ->
        Some
          ( before
          , after
            |> of_string)
      end

  let explode p =
    if is_root p then []
    else String.split (to_string p) ~on:'/'

  let to_string_maybe_quoted t =
    String.maybe_quoted (to_string t)

  module Set = struct
    include T.Set
    let of_listing ~dir ~filenames =
      of_list (List.map filenames ~f:(fun f -> relative dir f))
  end

  module Map = T.Map
  module Table = Hashtbl.Make(T)
end

module Local : sig
  include Path_intf.Local

  val root : t
  val is_root : t -> bool
  val compare_val : t -> t -> Ordering.t
  val relative : ?error_loc:Loc0.t -> t -> string -> t
  val append : t -> t -> t
  val mkdir_p : t -> unit
  val descendant : t -> of_:t -> t option
  val is_descendant : t -> of_:t -> bool
  val reach : t -> from:t -> string

  val append_relative : t -> Relative.t -> t

  module L : sig
    val relative : ?error_loc:Loc0.t -> t -> string list -> t
  end

  module Prefix : sig
    type local = t
    type t

    val make : local -> t
    val drop : t -> local -> local option

    (* for all local path p, drop (invalid p = None) *)
    val invalid : t
  end with type local := t

  val split_first_component : t -> (string * t) option
  val explode : t -> string list
  val of_local : t -> t
  val of_relative : Relative.t -> t

end = struct
  (* either "." for root, or a '/' separated list of components
     other that ".", ".."  and not containing '/'. *)
  module T = Interned.No_interning(struct
      let initial_size = 512
      let resize_policy = Interned.Greedy
      let order = Interned.Natural
    end)()

  module Table = Hashtbl.Make(T)

  type t = T.t

  let to_string = T.to_string
  let make = T.make
  let hash = T.hash
  let compare = T.compare

  let pp ppf s = Format.pp_print_string ppf (to_string s)

  let compare_val x y = String.compare (to_string x) (to_string y)

  let root = make "."

  let is_root t = Ordering.is_eq (compare t root)

  let is_suffix t ~suffix = String.is_suffix (to_string t) ~suffix

  let to_list =
    let rec loop t acc i j =
      if i = 0 then
        String.take t j :: acc
      else
        match t.[i - 1] with
        | '/' -> loop t (String.sub t ~pos:i ~len:(j - i) :: acc) (i - 1) (i - 1)
        | _   -> loop t acc (i - 1) j
    in
    fun t ->
      if is_root t then
        []
      else
        let t = to_string t in
        let len = String.length t in
        loop t [] len len

  let parent t =
    if is_root t then
      None
    else
      let t = to_string t in
      match String.rindex_from t (String.length t - 1) '/' with
      | exception Not_found -> Some root
      | i -> Some (make (String.take t i))

  let parent_exn t =
    match parent t with
    | None -> Exn.code_error "Path.Local.parent called on the root" []
    | Some t -> t

  let basename t =
    if is_root t then
      Exn.code_error "Path.Local.basename called on the root" []
    else
      let t = to_string t in
      let len = String.length t in
      match String.rindex_from t (len - 1) '/' with
      | exception Not_found -> t
      | i -> String.sub t ~pos:(i + 1) ~len:(len - i - 1)

  let to_sexp t = Sexp.Encoder.string (to_string t)
  let to_dyn t = Dyn.String (to_string t)

  module L = struct
    let relative_result t components =
      let rec loop t components =
        match components with
        | [] -> Result.Ok t
        | "." :: rest -> loop t rest
        | ".." :: rest ->
          if is_root t then
            Result.Error ()
          else
            begin match parent t with
            | None -> Error ()
            | Some parent -> loop parent rest
            end
        | fn :: rest ->
          if is_root t then
            loop (make fn) rest
          else
            loop (make (to_string t ^ "/" ^ fn)) rest
      in loop t components

    let relative ?error_loc t components =
      match relative_result t components with
      | Result.Ok t -> t
      | Error () ->
        Exn.fatalf ?loc:error_loc "path outside the workspace: %s from %s"
          (String.concat ~sep:"/" components)
          (to_string t)
  end

  let relative ?error_loc t path =
    if not (Filename.is_relative path) then (
      Exn.code_error "Local.relative: received absolute path"
        [ "t", to_sexp t
        ; "path", Sexp.Encoder.string path
        ]
    );
    match L.relative_result t (explode_path path) with
    | Result.Ok t -> t
    | Error () ->
      Exn.fatalf ?loc:error_loc "path outside the workspace: %s from %s"
        path
        (to_string t)

  let append_relative t rel = relative t (Relative.to_string rel)

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
      len = 0 || before_slash s (len - 1)

  let parse_string_exn ~loc s =
    match s with
    | "" | "." -> root
    | _ when is_canonicalized s -> make s
    | _ -> relative root s ~error_loc:loc
  let of_string s = parse_string_exn ~loc:Loc0.none s

  let rec mkdir_p t =
    if is_root t then
      ()
    else
      let t_s = to_string t in
      try
        Unix.mkdir t_s 0o777
      with
      | Unix.Unix_error (EEXIST, _, _) -> ()
      | Unix.Unix_error (ENOENT, _, _) as e ->
        let parent = parent_exn t in
        if is_root parent then
          raise e
        else begin
          mkdir_p parent;
          Unix.mkdir t_s 0o777
        end

  let append a b =
    match is_root a, is_root b with
    | true, _ -> b
    | _, true -> a
    | _, _ -> make ((to_string a) ^ "/" ^ (to_string b))

  let descendant t ~of_ =
    if is_root of_ then
      Some t
    else if t = of_ then
      Some root
    else
      let t = to_string t in
      let of_ = to_string of_ in
      let of_len = String.length of_ in
      let t_len = String.length t in
      if (t_len > of_len && t.[of_len] = '/'
          && String.is_prefix t ~prefix:of_) then
        Some (make (String.drop t (of_len + 1)))
      else
        None

  let is_descendant t ~of_ =
    is_root of_
    || t = of_
    || (
      let t = to_string t in
      let of_ = to_string of_ in
      let of_len = String.length of_ in
      let t_len = String.length t in
      (t_len > of_len && t.[of_len] = '/' && String.is_prefix t ~prefix:of_))

  let reach t ~from =
    let rec loop t from =
      match t, from with
      | a :: t, b :: from when a = b ->
        loop t from
      | _ ->
        match List.fold_left from ~init:t ~f:(fun acc _ -> ".." :: acc) with
        | [] -> "."
        | l -> (String.concat l ~sep:"/")
    in
    loop (to_list t) (to_list from)

  let extend_basename t ~suffix = make (to_string t ^ suffix)

  let extension t = Filename.extension (to_string t)
  let split_extension t =
    let s, ext = Filename.split_extension (to_string t) in
    (make s, ext)

  let set_extension t ~ext =
    let (base, _) = split_extension t in
    (to_string base) ^ ext
    |> make

  module Prefix = struct
    let make_path = make

    type t =
      { len        : int
      ; path       : string
      ; path_slash : string
      }

    let make p =
      if is_root p then
        Exn.code_error "Path.Local.Prefix.make"
          [ "path", to_sexp p ];
      let p = to_string p in
      { len        = String.length p
      ; path       = p
      ; path_slash = p ^ "/"
      }

    let drop t p =
      let p = to_string p in
      let len = String.length p in
      if len = t.len && p = t.path then
        Some root
      else
        String.drop_prefix p ~prefix:t.path_slash
        |> Option.map ~f:make_path

    let invalid =
      { len        = -1
      ; path       = "/"
      ; path_slash = "/"
      }
  end

  include (
    Comparable.Operators(struct
      type nonrec t = t
      let compare = compare_val
    end)
    : Comparable.OPS with type t := t
  )

  let split_first_component t =
    if is_root t then None
    else
      let t = (to_string t) in
      begin match String.lsplit2 t ~on:'/' with
      | None -> Some (t, root)
      | Some (before, after) ->
        Some (before, of_string after)
      end

  let explode p =
    if is_root p then []
    else String.split (to_string p) ~on:'/'

  let to_string_maybe_quoted t =
    String.maybe_quoted (to_string t)

  let parent_exn t =
    match parent t with
    | None -> Exn.code_error "Path.Local.parent:exn t is root"
                ["t", to_sexp t]
    | Some parent -> parent

  let of_local t = t
  let of_relative t = of_string (Relative.to_string t)

  module Set = struct
    include T.Set
    let of_listing ~dir ~filenames =
      of_list (List.map filenames ~f:(fun f -> relative dir f))
  end

  module Map = T.Map
end

module Source0 = Local

let (abs_root, set_root) =
  let root_dir = ref None in
  let set_root new_root =
    match !root_dir with
    | None -> root_dir := Some new_root
    | Some root_dir ->
      Exn.code_error "set_root: cannot set root_dir more than once"
        [ "root_dir", External.to_sexp root_dir
        ; "new_root_dir", External.to_sexp new_root
        ]
  in
  let abs_root = lazy (
    match !root_dir with
    | None ->
      Exn.code_error "root_dir: cannot use root dir before it's set" []
    | Some root_dir -> root_dir)
  in
  (abs_root, set_root)


module Kind = struct
  type t =
    | External of External.t
    | Local    of Local.t

  let to_absolute_filename t =
    match t with
    | External s -> External.to_string s
    | Local l ->
      External.to_string
        (External.relative (Lazy.force abs_root)
           (Local.to_string l))

  let to_string = function
    | Local t -> Local.to_string t
    | External t -> External.to_string t

  let to_sexp t = Sexp.Encoder.string (to_string t)

  let of_string s =
    if Filename.is_relative s then
      Local (Local.of_string s)
    else
      External (External.of_string s)

  let _ =
    let root = Local Local.root in
    assert (of_string ""  = root);
    assert (of_string "." = root)

  let _relative ?error_loc t fn =
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


module Build = struct
  include Local
  let append_source = append
  let append_relative = append_relative
  let append_local = append

  let local t = t

  let extract_build_context t =
    let t = Local.to_string t in
    begin match String.lsplit2 t ~on:'/' with
    | None ->
      Some (t, Source0.root)
    | Some (before, after) ->
      Some
        ( before
        , after
          |> Source0.of_string )
    end

  let extract_build_context_dir t =
    let t_str = Local.to_string t in
    begin match String.lsplit2 t_str ~on:'/' with
    | None -> Some (t, Source0.root)
    | Some (before, after) ->
      Some
        ( Local.of_string before
        , after
          |> Source0.of_string
        )
    end

  let extract_build_context_dir_exn t =
    match extract_build_context_dir t with
    | Some t -> t
    | None -> Exn.code_error "Path.Build.extract_build_context_dir_exn"
                ["t", to_sexp t]

  let extract_build_context_exn t =
    match extract_build_context t with
    | Some t -> t
    | None -> Exn.code_error "Path.Build.extract_build_context_exn"
                ["t", to_sexp t]

  let drop_build_context t = Option.map (extract_build_context t) ~f:snd
  let drop_build_context_exn t =
    match drop_build_context t with
    | Some d -> d
    | None ->
      Exn.code_error "Path.Build.drop_build_context_exn"
        [ "t", to_sexp t
        ]

  let is_alias_stamp_file s =
    String.is_prefix (Local.to_string s) ~prefix:".aliases/"

  let (build_dir_kind, build_dir_prefix, set_build_dir) =
    let build_dir = ref None in
    let build_dir_prefix = ref None in
    let set_build_dir (new_build_dir : Kind.t) =
      match !build_dir with
      | None ->
        (match new_build_dir with
         | External _ -> ()
         | Local p ->
           if Local.is_root p || Local.parent_exn p <> Local.root then
             Exn.fatalf
               "@{<error>Error@}: Invalid build directory: %s\n\
                The build directory must be an absolute path or \
                a sub-directory of the root of the workspace."
               (Local.to_string p |> String.maybe_quoted));
        build_dir := Some new_build_dir;
        build_dir_prefix :=
          Some (match new_build_dir with
            | Local    p -> Local.Prefix.make p
            | External _ -> Local.Prefix.invalid)
      | Some build_dir ->
        Exn.code_error "set_build_dir: cannot set build_dir more than once"
          [ "build_dir", Kind.to_sexp build_dir
          ; "new_build_dir", Kind.to_sexp new_build_dir ]
    in
    let build_dir = lazy (
      match !build_dir with
      | None ->
        Exn.code_error "build_dir: cannot use build dir before it's set" []
      | Some build_dir -> build_dir)
    in
    let build_dir_prefix = lazy (
      match !build_dir_prefix with
      | None ->
        Exn.code_error "build_dir: cannot use build dir before it's set" []
      | Some prefix -> prefix)
    in
    (build_dir, build_dir_prefix, set_build_dir)

  let to_string p =
    match Lazy.force build_dir_kind with
    | Local    b -> Local.to_string (Local.append b p)
    | External b ->
      if Local.is_root p then
        External.to_string b
      else
        Filename.concat (External.to_string b) (Local.to_string p)
end

module T : sig
  type t = private
    | External of External.t
    | In_source_tree of Local.t
    | In_build_dir of Local.t

  val compare : t -> t -> Ordering.t
  val equal : t -> t -> bool
  val hash : t -> int

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

  let equal (x : t) (y : t) = x = y
  let hash = Hashtbl.hash

  let in_build_dir s = In_build_dir s
  let in_source_tree s = In_source_tree s
  let external_ e = External e
end

include T

let hash (t : t) = Hashtbl.hash t

let build_dir = in_build_dir Local.root

let is_root = function
  | In_source_tree s -> Local.is_root s
  | In_build_dir _
  | External _  -> false

module Map = Map.Make(T)

let kind = function
  | In_build_dir p -> Kind.append_local (Lazy.force Build.build_dir_kind) p
  | In_source_tree s -> Kind.Local s
  | External s -> Kind.External s

let is_managed = function
  | In_build_dir _
  | In_source_tree _ -> true
  | External _ -> false

let to_string t =
  match t with
  | In_source_tree p -> Local.to_string p
  | External       p -> External.to_string p
  | In_build_dir   p -> Build.to_string p

let to_string_maybe_quoted t =
  String.maybe_quoted (to_string t)

let root = in_source_tree Local.root

let make_local_path p =
  match Local.Prefix.drop (Lazy.force Build.build_dir_prefix) p with
  | None -> in_source_tree p
  | Some p -> in_build_dir p
let of_local = make_local_path

let relative ?error_loc t fn =
  match fn with
  | "" | "." ->
    t
  | _ when not (Filename.is_relative fn) ->
    external_ (External.of_string fn)
  |_ ->
    match t with
    | In_source_tree p -> make_local_path (Local.relative p fn ?error_loc)
    | In_build_dir p -> in_build_dir (Local.relative p fn ?error_loc)
    | External s -> external_ (External.relative s fn)

let parse_string_exn ~loc s =
  match s with
  | "" | "." -> in_source_tree Local.root
  | s  ->
    if Filename.is_relative s then
      make_local_path (Local.parse_string_exn ~loc s)
    else
      external_ (External.parse_string_exn ~loc s)

let of_string s = parse_string_exn ~loc:Loc0.none s

let to_sexp t =
  let constr f x y = Sexp.Encoder.(pair string f) (x, y) in
  match t with
  | In_build_dir s -> constr Local.to_sexp "In_build_dir" s
  | In_source_tree s -> constr Local.to_sexp "In_source_tree" s
  | External s -> constr External.to_sexp "External" s

let to_dyn t =
  let open Dyn in
  match t with
  | In_build_dir s -> Variant ("In_build_dir", [Local.to_dyn s])
  | In_source_tree s -> Variant ("In_source_tree", [Local.to_dyn s])
  | External s -> Variant ("External", [External.to_dyn s])

let of_filename_relative_to_initial_cwd fn =
  external_ (
    if Filename.is_relative fn then
      External.relative External.initial_cwd fn
    else
      External.of_string fn
  )

let to_absolute_filename t = Kind.to_absolute_filename (kind t)

let external_of_local x ~root =
  External.to_string (External.relative root (Local.to_string x))

let external_of_in_source_tree x =
  external_of_local x ~root:(Lazy.force abs_root)

let reach t ~from =
  match t, from with
  | External t, _ -> External.to_string t
  | In_source_tree t, In_source_tree from
  | In_build_dir   t, In_build_dir   from -> Local.reach t ~from
  | In_source_tree t, In_build_dir from -> begin
      match Lazy.force Build.build_dir_kind with
      | Local    b -> Local.reach t ~from:(Local.append b from)
      | External _ -> external_of_in_source_tree t
    end
  | In_build_dir t, In_source_tree from -> begin
      match Lazy.force Build.build_dir_kind with
      | Local    b -> Local.reach (Local.append b t) ~from
      | External b -> external_of_local t ~root:b
    end
  | In_source_tree t, External _ -> external_of_in_source_tree t
  | In_build_dir t, External _ ->
    match Lazy.force Build.build_dir_kind with
    | Local    b -> external_of_in_source_tree (Local.append b t)
    | External b -> external_of_local t ~root:b

let reach_for_running ?(from=root) t =
  let fn = reach t ~from in
  match Filename.analyze_program_name fn with
  | In_path -> "./" ^ fn
  | _       -> fn

let descendant t ~of_ =
  match t, of_ with
  | In_source_tree t, In_source_tree of_
  | In_build_dir t, In_build_dir of_ ->
    Option.map ~f:in_source_tree (Local.descendant t ~of_)
  | _ -> None

let is_descendant t ~of_ =
  match t, of_ with
  | In_source_tree t, In_source_tree of_
  | In_build_dir t, In_build_dir of_ ->
    Local.is_descendant t ~of_
  | _ -> false

let append_relative a b =
  match a with
  | In_source_tree a -> in_source_tree (Local.append_relative a b)
  | In_build_dir a -> in_build_dir (Local.append_relative a b)
  | External a -> external_ (External.relative a (Relative.to_string b))

let append_local a b =
  match a with
  | In_source_tree a -> in_source_tree (Local.append a b)
  | In_build_dir a -> in_build_dir (Local.append a b)
  | External a -> external_ (External.relative a (Local.to_string b))

let append_local = append_local
let append_source = append_local

let append a b =
  match b with
  | In_build_dir _ | External _ ->
    Exn.code_error "Path.append called with directory that's \
                    not in the source tree"
      [ "a", to_sexp a
      ; "b", to_sexp b
      ]
  | In_source_tree b -> append_local a b

let basename t =
  match kind t with
  | Local t -> Local.basename t
  | External t -> External.basename t

let parent = function
  | External s -> Option.map (External.parent s) ~f:external_
  | In_source_tree l ->
    Local.parent l
    |> Option.map ~f:in_source_tree
  | In_build_dir l ->
    Local.parent l
    |> Option.map ~f:in_build_dir

let parent_exn t =
  match parent t with
  | Some p -> p
  | None -> Exn.code_error "Path.parent:exn t is root"
              ["t", to_sexp t]

let is_strict_descendant_of_build_dir = function
  | In_build_dir p -> not (Local.is_root p)
  | In_source_tree _
  | External _ -> false

let is_in_build_dir = function
  | In_build_dir _ -> true
  | In_source_tree _
  | External _ -> false

let is_in_source_tree = function
  | In_source_tree _ -> true
  | In_build_dir _
  | External _ -> false

let as_in_source_tree = function
  | In_source_tree s -> Some s
  | In_build_dir _
  | External _ -> None

let as_in_build_dir = function
  | In_build_dir b -> Some b
  | In_source_tree _
  | External _ -> None

let as_in_build_dir_exn t = match t with
  | External _ | In_source_tree _  ->
    Exn.code_error
      "[as_in_build_dir_exn] called on something not in build dir"
      ["t", to_sexp t]
  | In_build_dir p -> p

let extract_build_context = function
  | In_source_tree _
  | External _ -> None
  | In_build_dir p when Local.is_root p -> None
  | In_build_dir t -> Build.extract_build_context t

let extract_build_dir_first_component = extract_build_context

let extract_build_context_exn t =
  match extract_build_context t with
  | Some t -> t
  | None -> Exn.code_error "Path.extract_build_context_exn"
              ["t", to_sexp t]

let extract_build_context_dir = function
  | In_source_tree _
  | External _ -> None
  | In_build_dir t ->
    Option.map (Build.extract_build_context_dir t)
      ~f:(fun (base, rest) -> in_build_dir base, rest)

let extract_build_context_dir_exn t =
  match extract_build_context_dir t with
  | Some t -> t
  | None -> Exn.code_error "Path.extract_build_context_dir_exn"
              ["t", to_sexp t]

let drop_build_context t =
  Option.map (extract_build_context t) ~f:snd

let drop_build_context_exn t =
  match extract_build_context t with
  | None -> Exn.code_error "Path.drop_build_context_exn" [ "t", to_sexp t ]
  | Some (_, t) -> t

let drop_optional_build_context t =
  match extract_build_context t with
  | None -> t
  | Some (_, t) -> in_source_tree t

let drop_optional_build_context_src_exn t =
  match t with
  | External _ ->
    Exn.code_error "drop_optional_build_context_src_exn called on an external path" []
  | In_build_dir _ ->
    (match extract_build_context t with
     | Some (_, s) -> s
     | None ->
       Exn.code_error
         "drop_optional_build_context_src_exn called on a build directory itself" [])
  | In_source_tree p -> p

let local_src   = Local.of_string "src"
let local_build = Local.of_string "build"

let sandbox_managed_paths =
  let append_local ~sandbox_dir local_x p =
    in_build_dir (Build.append_local sandbox_dir (Local.append local_x p))
  in
  fun ~(sandbox_dir : Build.t) t ->
    match t with
    | External _ -> t
    | In_source_tree p -> append_local ~sandbox_dir local_src p
    | In_build_dir   p -> append_local ~sandbox_dir local_build p

let split_first_component t =
  match kind t, is_root t with
  | Local t, false ->
    let t = Local.to_string t in
    begin match String.lsplit2 t ~on:'/' with
    | None -> Some (t, root)
    | Some (before, after) ->
      Some
        ( before
        , after
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
              ["path", to_sexp t]

let exists t =
  try Sys.file_exists (to_string t)
  with Sys_error _ -> false
let readdir_unsorted =
  let rec loop dh acc =
    match Unix.readdir dh with
    | "."
    | ".." -> loop dh acc
    | s -> loop dh (s :: acc)
    | exception End_of_file -> acc
  in
  fun t ->
    try
      let dh = Unix.opendir (to_string t) in
      Exn.protect
        ~f:(fun () ->
          match loop dh [] with
          | exception (Unix.Unix_error (e, _, _)) -> Error e
          | s -> Result.Ok s)
        ~finally:(fun () -> Unix.closedir dh)
    with
      Unix.Unix_error (e, _, _) -> Error e

let is_directory t =
  try Sys.is_directory (to_string t)
  with Sys_error _ -> false
let is_file t = not (is_directory t)
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
      [ "path"  , to_sexp a
      ; "insert", Sexp.Encoder.string b
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
        ["t", to_sexp t]
    );
    let fn = to_string t in
    match Unix.lstat fn with
    | exception Unix.Unix_error(ENOENT, _, _) -> ()
    | _ -> loop fn

let mkdir_p = function
  | External s -> External.mkdir_p s
  | In_source_tree s ->
    Local.mkdir_p s
  | In_build_dir k ->
    Kind.mkdir_p (Kind.append_local (Lazy.force Build.build_dir_kind) k)

let compare x y =
  match x, y with
  | External x      , External y       -> External.compare_val x y
  | External _      , _                -> Lt
  | _               , External _       -> Gt
  | In_source_tree x, In_source_tree y -> Local.compare_val x y
  | In_source_tree _, _                -> Lt
  | _               , In_source_tree _ -> Gt
  | In_build_dir x  , In_build_dir y   -> Local.compare_val x y

let extension t =
  match t with
  | External t -> External.extension t
  | In_build_dir t | In_source_tree t -> Local.extension t

let split_extension t =
  match t with
  | External t ->
    let t, ext = External.split_extension t in
    (external_ t, ext)
  | In_build_dir t ->
    let t, ext = Local.split_extension t in
    (in_build_dir t, ext)
  | In_source_tree t ->
    let t, ext = Local.split_extension t in
    (in_source_tree t, ext)

let set_extension t ~ext =
  match t with
  | External t -> external_ (External.set_extension t ~ext)
  | In_build_dir t -> in_build_dir (Local.set_extension t ~ext)
  | In_source_tree t -> in_source_tree (Local.set_extension t ~ext)

let pp ppf t = Format.pp_print_string ppf (to_string_maybe_quoted t)

let pp_in_source ppf t =
  pp ppf (drop_optional_build_context t)

let pp_debug ppf = function
  | In_source_tree s ->
    Format.fprintf ppf "(In_source_tree %S)" (Local.to_string s)
  | In_build_dir s ->
    Format.fprintf ppf "(In_build_dir %S)" (Local.to_string s)
  | External s -> Format.fprintf ppf "(External %S)" (External.to_string s)

module Set = struct
  include Set.Make(T)
  let to_sexp t = Sexp.Encoder.(list to_sexp) (to_list t)
  let of_listing ~dir ~filenames =
    of_list (List.map filenames ~f:(fun f -> relative dir f))
end

let in_source s = in_source_tree (Local.of_string s)
let source s = in_source_tree s
let build s = in_build_dir s

let is_suffix p ~suffix =
  match p with
  | In_build_dir l
  | In_source_tree l -> Local.is_suffix l ~suffix
  | External p -> External.is_suffix p ~suffix

module Table = Hashtbl.Make(T)

module Internal = struct
  let raw_kind = function
    | In_build_dir l -> Kind.Local l
    | In_source_tree l -> Local l
    | External l -> External l
end

module L = struct
  (* TODO more efficient implementation *)
  let relative t = List.fold_left ~init:t ~f:relative
end

let local_part = function
  | External e -> Local.of_string (External.as_local e)
  | In_source_tree l -> l
  | In_build_dir l -> l

let stat t = Unix.stat (to_string t)

include (Comparable.Operators(T) : Comparable.OPS with type t := t)

let path_of_local = of_local

module Source = struct
  let is_in_build_dir s =
    is_in_build_dir (path_of_local s)

  include Local

  let to_local t = t
end

let set_of_source_paths set =
  Source.Set.to_list set
  |> List.map ~f:source
  |> Set.of_list

let set_of_build_paths_list =
  List.fold_left ~init:Set.empty ~f:(fun acc e -> Set.add acc (build e))
