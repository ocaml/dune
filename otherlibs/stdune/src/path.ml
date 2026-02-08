module Sys = Stdlib.Sys
include Path0
module External = Path_external

module Relative_to_source_root = struct
  let mkdir_p ?perms path =
    ignore (Fpath.mkdir_p ?perms (Local.to_string path) : Fpath.mkdir_p_result)
  ;;
end

module Source0 = struct
  include Local

  let to_dyn s = Dyn.variant "In_source_tree" [ to_dyn s ]
  let append_local = Local.append
end

let abs_root, set_root =
  let root_dir = ref None in
  let set_root new_root =
    match !root_dir with
    | None -> root_dir := Some new_root
    | Some root_dir ->
      Code_error.raise
        "set_root: cannot set root_dir more than once"
        [ "root_dir", External.to_dyn root_dir; "new_root_dir", External.to_dyn new_root ]
  in
  let abs_root =
    lazy
      (match !root_dir with
       | None -> Code_error.raise "root_dir: cannot use root dir before it's set" []
       | Some root_dir -> root_dir)
  in
  abs_root, set_root
;;

module Outside_build_dir = struct
  type t =
    | External of External.t
    | In_source_dir of Local.t

  let to_absolute_filename t =
    match t with
    | External s -> External.to_string s
    | In_source_dir l ->
      External.to_string (External.relative (Lazy.force abs_root) (Local.to_string l))
  ;;

  let to_string = function
    | In_source_dir t -> Local.to_string t
    | External t -> External.to_string t
  ;;

  let to_dyn = function
    | In_source_dir t -> Source0.to_dyn t
    | External t -> External.to_dyn t
  ;;

  let of_string s =
    if Filename.is_relative s
    then In_source_dir (Local.of_string s)
    else External (External.of_string s)
  ;;

  let mkdir_p ?perms = function
    | In_source_dir t -> Relative_to_source_root.mkdir_p ?perms t
    | External t ->
      let (_ : Fpath.mkdir_p_result) = Fpath.mkdir_p ?perms (External.to_string t) in
      ()
  ;;

  let relative t s =
    match t with
    | In_source_dir t -> In_source_dir (Local.relative t s)
    | External t -> External (External.relative t s)
  ;;

  let extend_basename t ~suffix =
    match t with
    | In_source_dir t -> In_source_dir (Local.extend_basename t ~suffix)
    | External t -> External (External.extend_basename t ~suffix)
  ;;

  let append_local x y =
    match x with
    | In_source_dir x -> In_source_dir (Local.append x y)
    | External x -> External (External.relative x (Local.to_string y))
  ;;

  let to_string_maybe_quoted t = String.maybe_quoted (to_string t)

  let equal (x : t) (y : t) =
    match x, y with
    | External x, External y -> External.equal x y
    | External _, In_source_dir _ -> false
    | In_source_dir x, In_source_dir y -> Local.equal x y
    | In_source_dir _, External _ -> false
  ;;

  let hash = Poly.hash

  let parent = function
    | In_source_dir t ->
      (match Local.parent t with
       | None -> None
       | Some s -> Some (In_source_dir s))
    | External t ->
      (match External.parent t with
       | None -> None
       | Some s -> Some (External s))
  ;;

  module Table = Hashtbl.Make (struct
      type nonrec t = t

      let hash = Poly.hash
      let equal = Poly.equal
      let to_dyn = to_dyn
    end)
end

module Permissions = struct
  type t =
    { current_user : int
    ; all_users : int
    }

  let execute = { current_user = 0o100; all_users = 0o111 }
  let write = { current_user = 0o200; all_users = 0o222 }
  let add t perm = perm lor t.current_user
  let test t perm = perm land t.current_user <> 0
  let remove t perm = perm land lnot t.all_users
end

module Build = struct
  include Local

  let append_source = append
  let append_local = append
  let local t = t
  let extract_build_context t = split_first_component t
  let extract_first_component = extract_build_context

  let extract_build_context_dir t =
    Option.map (split_first_component t) ~f:(fun (before, after) ->
      Local.of_string before, after)
  ;;

  let split_sandbox_root t_original =
    match split_first_component t_original with
    | Some (".sandbox", t) ->
      (match split_first_component t with
       | Some (sandbox_name, t) -> Some (of_string (".sandbox" ^ "/" ^ sandbox_name)), t
       | None -> None, t_original)
    | Some _ | None -> None, t_original
  ;;

  let extract_build_context_dir_maybe_sandboxed t =
    let sandbox_root, t = split_sandbox_root t in
    Option.map (extract_build_context_dir t) ~f:(fun (ctx_dir, src_dir) ->
      let ctx_dir =
        match sandbox_root with
        | None -> ctx_dir
        | Some root -> append root ctx_dir
      in
      ctx_dir, src_dir)
  ;;

  let extract_build_context_dir_exn t =
    match extract_build_context_dir t with
    | Some t -> t
    | None ->
      Code_error.raise "Path.Build.extract_build_context_dir_exn" [ "t", to_dyn t ]
  ;;

  let extract_build_context_exn t =
    match extract_build_context t with
    | Some t -> t
    | None -> Code_error.raise "Path.Build.extract_build_context_exn" [ "t", to_dyn t ]
  ;;

  let drop_build_context t = Option.map (extract_build_context t) ~f:snd

  let drop_build_context_exn t =
    match drop_build_context t with
    | Some d -> d
    | None -> Code_error.raise "Path.Build.drop_build_context_exn" [ "t", to_dyn t ]
  ;;

  let drop_build_context_maybe_sandboxed_exn t =
    match extract_build_context_dir_maybe_sandboxed t with
    | Some (_, t) -> t
    | None ->
      Code_error.raise
        "Path.Build.drop_build_context_maybe_sandboxed_exn"
        [ "t", to_dyn t ]
  ;;

  let build_dir = Fdecl.create Outside_build_dir.to_dyn
  let build_dir_prefix = Fdecl.create Dyn.opaque

  let set_build_dir (new_build_dir : Outside_build_dir.t) =
    let new_build_dir_prefix =
      (match new_build_dir with
       | External _ -> ()
       | In_source_dir p ->
         if Local.is_root p || Local.parent_exn p <> Local.root
         then
           User_error.raise
             [ Pp.textf
                 "Invalid build directory: %s"
                 (Local.to_string p |> String.maybe_quoted)
             ; Pp.text
                 "The build directory must be an absolute path or a sub-directory of the \
                  root of the workspace."
             ]);
      match new_build_dir with
      | In_source_dir p -> Local.Prefix.make p
      | External _ -> Local.Prefix.invalid
    in
    Fdecl.set build_dir new_build_dir;
    Fdecl.set build_dir_prefix new_build_dir_prefix
  ;;

  let to_string p =
    match Fdecl.get build_dir with
    | In_source_dir b -> Local.to_string (Local.append b p)
    | External b ->
      if Local.is_root p
      then External.to_string b
      else Filename.concat (External.to_string b) (Local.to_string p)
  ;;

  let to_string_maybe_quoted p = String.maybe_quoted (to_string p)
  let of_local t = t
  let to_dyn s = Dyn.variant "In_build_dir" [ to_dyn s ]
end

module T : sig
  type t =
    | External of External.t
    | In_source_tree of Local.t
    | In_build_dir of Local.t

  val to_dyn : t -> Dyn.t

  (** [External _] < [In_source_tree _] < [In_build_dir _]

      Path of the same kind are compared using the standard lexical order *)
  val compare : t -> t -> Ordering.t

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
    | External x, External y -> External.compare x y
    | External _, _ -> Lt
    | _, External _ -> Gt
    | In_source_tree x, In_source_tree y -> Local.compare x y
    | In_source_tree _, In_build_dir _ -> Lt
    | In_build_dir _, In_source_tree _ -> Gt
    | In_build_dir x, In_build_dir y -> Local.compare x y
  ;;

  let hash = Poly.hash
  let in_build_dir s = In_build_dir s
  let in_source_tree s = In_source_tree s
  let external_ e = External e

  let to_dyn = function
    | In_build_dir s -> Build.to_dyn s
    | In_source_tree s -> Source0.to_dyn s
    | External s -> External.to_dyn s
  ;;
end

include T

let build_dir = in_build_dir Local.root

let is_root = function
  | In_source_tree s -> Local.is_root s
  | In_build_dir _ | External _ -> false
;;

let local_or_external : t -> Outside_build_dir.t = function
  | In_build_dir p -> Outside_build_dir.append_local (Fdecl.get Build.build_dir) p
  | In_source_tree s -> In_source_dir s
  | External s -> External s
;;

let is_managed = function
  | In_build_dir _ | In_source_tree _ -> true
  | External _ -> false
;;

let to_string t =
  match t with
  | In_source_tree p -> Local.to_string p
  | External p -> External.to_string p
  | In_build_dir p -> Build.to_string p
;;

let to_string_maybe_quoted t = String.maybe_quoted (to_string t)
let root = in_source_tree Local.root

let make_local_path p =
  match Local.Prefix.drop (Fdecl.get Build.build_dir_prefix) p with
  | None -> in_source_tree p
  | Some p -> in_build_dir p
;;

let of_local = make_local_path

let relative ?error_loc t fn =
  match fn with
  | "" | "." -> t
  | _ when not (Filename.is_relative fn) -> external_ (External.of_string fn)
  | _ ->
    (match t with
     | In_source_tree p ->
       let fn' = explode_path fn in
       (match Local.L.relative_result p fn' with
        | Ok l -> make_local_path l
        | Error `Outside_the_workspace ->
          external_
            (External.relative
               (External.of_string
                  (Outside_build_dir.to_absolute_filename (local_or_external t)))
               fn))
     | In_build_dir p -> in_build_dir (Local.relative p fn ?error_loc)
     | External s -> external_ (External.relative s fn))
;;

let parse_string_exn ~loc s =
  match s with
  | "" | "." -> in_source_tree Local.root
  | s ->
    if Filename.is_relative s
    then make_local_path (Local.parse_string_exn ~loc s)
    else external_ (External.parse_string_exn ~loc s)
;;

let of_string s = parse_string_exn ~loc:Loc0.none s

let of_filename_relative_to_initial_cwd fn =
  external_ (External.of_filename_relative_to_initial_cwd fn)
;;

let of_string_allow_outside_workspace s =
  if Filename.is_relative s
  then (
    match Local.L.relative_result Local.root (explode_path s) with
    | Ok _ -> of_string s
    | Error `Outside_the_workspace -> of_filename_relative_to_initial_cwd s)
  else of_string s
;;

let to_absolute_filename t = Outside_build_dir.to_absolute_filename (local_or_external t)

let external_of_local x ~root =
  External.to_string (External.relative root (Local.to_string x))
;;

let external_of_in_source_tree x = external_of_local x ~root:(Lazy.force abs_root)

let reach t ~from =
  match t, from with
  | External t, _ -> External.to_string t
  | In_source_tree t, In_source_tree from | In_build_dir t, In_build_dir from ->
    Local.reach t ~from
  | In_source_tree t, In_build_dir from ->
    (match Fdecl.get Build.build_dir with
     | In_source_dir b -> Local.reach t ~from:(Local.append b from)
     | External _ -> external_of_in_source_tree t)
  | In_build_dir t, In_source_tree from ->
    (match Fdecl.get Build.build_dir with
     | In_source_dir b -> Local.reach (Local.append b t) ~from
     | External b -> external_of_local t ~root:b)
  | In_source_tree t, External _ -> external_of_in_source_tree t
  | In_build_dir t, External _ ->
    (match Fdecl.get Build.build_dir with
     | In_source_dir b -> external_of_in_source_tree (Local.append b t)
     | External b -> external_of_local t ~root:b)
;;

let reach_for_running ?(from = root) t =
  let fn = reach t ~from in
  match Filename.analyze_program_name fn with
  | In_path -> "./" ^ fn
  | _ -> fn
;;

let descendant t ~of_ =
  match t, of_ with
  | In_source_tree t, In_source_tree of_ | In_build_dir t, In_build_dir of_ ->
    Option.map ~f:in_source_tree (Local.descendant t ~of_)
  | _ -> None
;;

let is_descendant t ~of_ =
  match t, of_ with
  | In_source_tree t, In_source_tree of_ | In_build_dir t, In_build_dir of_ ->
    Local.is_descendant t ~of_
  | _ -> false
;;

let append_local a b =
  match a with
  | In_source_tree a -> in_source_tree (Local.append a b)
  | In_build_dir a -> in_build_dir (Local.append a b)
  | External a -> external_ (External.relative a (Local.to_string b))
;;

let append_local = append_local
let append_source = append_local

let basename t =
  match t with
  | In_build_dir p -> Local.basename p
  | In_source_tree s -> Local.basename s
  | External s -> External.basename s
;;

let is_a_root = function
  | In_build_dir p -> Local.is_root p
  | In_source_tree s -> Local.is_root s
  | External s -> External.is_root s
;;

let basename_opt = basename_opt ~is_root:is_a_root ~basename

let parent = function
  | External s -> Option.map (External.parent s) ~f:external_
  | In_source_tree l -> Local.parent l |> Option.map ~f:in_source_tree
  | In_build_dir l -> Local.parent l |> Option.map ~f:in_build_dir
;;

let parent_exn t =
  match parent t with
  | Some p -> p
  | None -> Code_error.raise "Path.parent:exn t is root" [ "t", to_dyn t ]
;;

let is_in_build_dir = function
  | In_build_dir _ -> true
  | In_source_tree _ | External _ -> false
;;

let is_in_source_tree = function
  | In_source_tree _ -> true
  | In_build_dir _ | External _ -> false
;;

let as_in_source_tree = function
  | In_source_tree s -> Some s
  | In_build_dir _ | External _ -> None
;;

let as_in_source_tree_exn t =
  match as_in_source_tree t with
  | Some t -> t
  | None ->
    Code_error.raise
      "[as_in_source_tree_exn] called on something not in source tree"
      [ "t", to_dyn t ]
;;

let as_outside_build_dir : t -> Outside_build_dir.t option = function
  | In_source_tree s -> Some (In_source_dir s)
  | External s -> Some (External s)
  | In_build_dir _ -> None
;;

let as_outside_build_dir_exn : t -> Outside_build_dir.t = function
  | In_source_tree s -> In_source_dir s
  | External s -> External s
  | In_build_dir path ->
    Code_error.raise "as_outside_build_dir_exn" [ "path", Build.to_dyn path ]
;;

let destruct_build_dir : t -> [ `Inside of Build.t | `Outside of Outside_build_dir.t ] =
  function
  | In_source_tree p -> `Outside (In_source_dir p)
  | External s -> `Outside (External s)
  | In_build_dir s -> `Inside s
;;

let outside_build_dir : Outside_build_dir.t -> t = function
  | In_source_dir d -> In_source_tree d
  | External s -> External s
;;

let as_in_build_dir = function
  | In_build_dir b -> Some b
  | In_source_tree _ | External _ -> None
;;

let as_in_build_dir_exn t =
  match t with
  | External _ | In_source_tree _ ->
    Code_error.raise
      "[as_in_build_dir_exn] called on something not in build dir"
      [ "t", to_dyn t ]
  | In_build_dir p -> p
;;

let as_external = function
  | External s -> Some s
  | In_build_dir _ | In_source_tree _ -> None
;;

let extract_build_context = function
  | In_source_tree _ | External _ -> None
  | In_build_dir p when Local.is_root p -> None
  | In_build_dir t -> Build.extract_build_context t
;;

let extract_build_context_exn t =
  match extract_build_context t with
  | Some t -> t
  | None -> Code_error.raise "Path.extract_build_context_exn" [ "t", to_dyn t ]
;;

let extract_build_context_dir = function
  | In_source_tree _ | External _ -> None
  | In_build_dir t ->
    Option.map (Build.extract_build_context_dir t) ~f:(fun (base, rest) ->
      in_build_dir base, rest)
;;

let extract_build_context_dir_maybe_sandboxed = function
  | In_source_tree _ | External _ -> None
  | In_build_dir t ->
    Option.map (Build.extract_build_context_dir_maybe_sandboxed t) ~f:(fun (base, rest) ->
      in_build_dir base, rest)
;;

let drop_optional_sandbox_root = function
  | (In_source_tree _ | External _) as x -> x
  | In_build_dir t ->
    (match Build.split_sandbox_root t with
     | _sandbox_root, t -> (In_build_dir t : t))
;;

let extract_build_context_dir_exn t =
  match extract_build_context_dir t with
  | Some t -> t
  | None -> Code_error.raise "Path.extract_build_context_dir_exn" [ "t", to_dyn t ]
;;

let drop_build_context t = Option.map (extract_build_context t) ~f:snd

let drop_build_context_exn t =
  match extract_build_context t with
  | None -> Code_error.raise "Path.drop_build_context_exn" [ "t", to_dyn t ]
  | Some (_, t) -> t
;;

let drop_optional_build_context t =
  match extract_build_context t with
  | None -> t
  | Some (_, t) -> in_source_tree t
;;

let pp path = drop_optional_build_context path |> to_string_maybe_quoted |> Pp.verbatim

let drop_optional_build_context_maybe_sandboxed t =
  match extract_build_context_dir_maybe_sandboxed t with
  | None -> t
  | Some (_, t) -> in_source_tree t
;;

let drop_optional_build_context_src_exn t =
  match t with
  | External _ ->
    Code_error.raise "drop_optional_build_context_src_exn called on an external path" []
  | In_build_dir _ ->
    (match extract_build_context t with
     | Some (_, s) -> s
     | None ->
       Code_error.raise
         "drop_optional_build_context_src_exn called on a build directory itself"
         [])
  | In_source_tree p -> p
;;

let split_first_component t =
  match local_or_external t with
  | In_source_dir t ->
    Option.map (Local.split_first_component t) ~f:(fun (before, after) ->
      before, after |> in_source_tree)
  | _ -> None
;;

let relative_to_source_in_build_or_external ?error_loc ~dir s =
  match Build.extract_build_context dir with
  | None -> relative ?error_loc (In_build_dir dir) s
  | Some (bctxt, source) ->
    let path = relative ?error_loc (In_source_tree source) s in
    (match path with
     | In_source_tree s ->
       In_build_dir (Build.relative (Build.of_string bctxt) (Source0.to_string s))
     | In_build_dir _ | External _ -> path)
;;

let readdir_unsorted t = Readdir.read_directory (to_string t)
let readdir_unsorted_with_kinds t = Readdir.read_directory_with_kinds (to_string t)
let build_dir_exists () = Fpath.is_directory (to_string build_dir)

let ensure_build_dir_exists () =
  let perms = 0o777 in
  match local_or_external build_dir with
  | In_source_dir p -> Relative_to_source_root.mkdir_p p ~perms
  | External p ->
    let p = External.to_string p in
    (match Fpath.mkdir ~perms p with
     | `Created | `Already_exists -> ()
     | `Missing_parent_directory ->
       User_error.raise
         [ Pp.textf
             "Cannot create external build directory %s. Make sure that the parent dir \
              %s exists."
             p
             (Filename.dirname p)
         ])
;;

let extend_basename t ~suffix =
  match t with
  | In_source_tree t -> in_source_tree (Local.extend_basename t ~suffix)
  | In_build_dir t -> in_build_dir (Local.extend_basename t ~suffix)
  | External t -> external_ (External.extend_basename t ~suffix)
;;

let rm_rf ?chmod ?(allow_external = false) t =
  if (not allow_external) && not (is_managed t)
  then Code_error.raise "Path.rm_rf called on external dir" [ "t", to_dyn t ];
  Fpath.rm_rf ?chmod (to_string t)
;;

let mkdir_p ?perms = function
  | External s ->
    let (_ : Fpath.mkdir_p_result) = Fpath.mkdir_p (External.to_string s) ?perms in
    ()
  | In_source_tree s -> Relative_to_source_root.mkdir_p s ?perms
  | In_build_dir k ->
    Outside_build_dir.mkdir_p
      ?perms
      (Outside_build_dir.append_local (Fdecl.get Build.build_dir) k)
;;

let extension t =
  match t with
  | External t -> External.extension t
  | In_build_dir t | In_source_tree t -> Local.extension t
;;

let split_extension t =
  match t with
  | External t ->
    let t, ext = External.split_extension t in
    external_ t, ext
  | In_build_dir t ->
    let t, ext = Local.split_extension t in
    in_build_dir t, ext
  | In_source_tree t ->
    let t, ext = Local.split_extension t in
    in_source_tree t, ext
;;

let set_extension t ~ext =
  match t with
  | External t -> external_ (External.set_extension t ~ext)
  | In_build_dir t -> in_build_dir (Local.set_extension t ~ext)
  | In_source_tree t -> in_source_tree (Local.set_extension t ~ext)
;;

let map_extension t ~f =
  let base, ext = split_extension t in
  extend_basename ~suffix:(Filename.Extension.Or_empty.to_string (f ext)) base
;;

module O = Comparable.Make (T)
module Map = O.Map

module Set = struct
  include O.Set

  let of_listing ~dir ~filenames = of_list_map filenames ~f:(fun f -> relative dir f)
end

let source s = in_source_tree s
let build s = in_build_dir s

module Table = struct
  type key = t

  type 'a t =
    { source : 'a Source0.Table.t
    ; build : 'a Build.Table.t
    ; external_ : 'a External.Table.t
    }

  let create () =
    { source = Source0.Table.create 0
    ; build = Build.Table.create 0
    ; external_ = External.Table.create 0
    }
  ;;

  let clear { source; build; external_ } =
    Source0.Table.clear source;
    Build.Table.clear build;
    External.Table.clear external_
  ;;

  let[@inline] mem { source; build; external_ } key =
    match key with
    | In_source_tree p -> Source0.Table.mem source p
    | In_build_dir p -> Build.Table.mem build p
    | External p -> External.Table.mem external_ p
  ;;

  let[@inline] set { source; build; external_ } k v =
    match k with
    | In_source_tree p -> Source0.Table.set source p v
    | In_build_dir p -> Build.Table.set build p v
    | External p -> External.Table.set external_ p v
  ;;

  let[@inline] remove { source; build; external_ } k =
    match k with
    | In_source_tree p -> Source0.Table.remove source p
    | In_build_dir p -> Build.Table.remove build p
    | External p -> External.Table.remove external_ p
  ;;

  let iter { source; build; external_ } ~f =
    Source0.Table.iter source ~f;
    Build.Table.iter build ~f;
    External.Table.iter external_ ~f
  ;;

  let[@inline] find { source; build; external_ } = function
    | In_source_tree p -> Source0.Table.find source p
    | In_build_dir p -> Build.Table.find build p
    | External p -> External.Table.find external_ p
  ;;

  let filteri_inplace { source; build; external_ } ~f =
    Source0.Table.filteri_inplace source ~f:(fun[@inline] ~key ~data ->
      f ~key:(In_source_tree key) ~data);
    Build.Table.filteri_inplace build ~f:(fun[@inline] ~key ~data ->
      f ~key:(In_build_dir key) ~data);
    External.Table.filteri_inplace external_ ~f:(fun[@inline] ~key ~data ->
      f ~key:(External key) ~data)
  ;;

  let filter_inplace { source; build; external_ } ~f =
    Source0.Table.filteri_inplace source ~f:(fun[@inline] ~key:_ ~data -> f data);
    Build.Table.filteri_inplace build ~f:(fun[@inline] ~key:_ ~data -> f data);
    External.Table.filteri_inplace external_ ~f:(fun[@inline] ~key:_ ~data -> f data)
  ;;

  let to_dyn f { source; build; external_ } =
    let open Dyn in
    record
      [ "source", Source0.Table.to_dyn f source
      ; "build", Build.Table.to_dyn f build
      ; "external_", External.Table.to_dyn f external_
      ]
  ;;
end

module L = struct
  (* TODO more efficient implementation *)
  let relative t = List.fold_left ~init:t ~f:relative
end

let local_part = function
  | External e -> Local.of_string (External.as_local e)
  | In_source_tree l -> l
  | In_build_dir l -> l
;;

let stat t = Unix_error.Detailed.catch (fun p -> Unix.stat (to_string p)) t
let lstat t = Unix_error.Detailed.catch (fun p -> Unix.lstat (to_string p)) t

include (Comparator.Operators (T) : Comparator.OPS with type t := t)

let path_of_local = of_local

module Source = struct
  include Source0

  let is_in_build_dir s = is_in_build_dir (path_of_local s)
  let to_local t = t
end

let drop_prefix path ~prefix =
  let prefix_s = to_string prefix in
  let path_s = to_string path in
  match Int.compare (String.length prefix_s) (String.length path_s) with
  | Eq -> if prefix = path then Some Local.root else None
  | Gt -> None
  | Lt ->
    let open Option.O in
    let* prefix =
      let* last = String.last prefix_s in
      if is_dir_sep last
      then Some prefix_s
      else if is_dir_sep path_s.[String.length prefix_s]
      then Some (prefix_s ^ String.make 1 path_s.[String.length prefix_s])
      else None
    in
    let+ suffix = String.drop_prefix path_s ~prefix in
    Local.of_string suffix
;;

let drop_prefix_exn t ~prefix =
  match drop_prefix t ~prefix with
  | None ->
    Code_error.raise "Path.drop_prefix_exn" [ "t", to_dyn t; "prefix", to_dyn prefix ]
  | Some p -> p
;;

let is_broken_symlink = function
  | External e -> Fpath.is_broken_symlink (External.to_string e)
  | In_source_tree _ | In_build_dir _ ->
    (* Paths within the source tree and build dir are always fully-expanded,
       so there's no possibility for them to be broken symlinks. *)
    false
;;

module Expert = struct
  let drop_absolute_prefix ~prefix p =
    match
      String.drop_prefix ~prefix:(Outside_build_dir.to_absolute_filename prefix) p
    with
    | None -> None
    | Some "" -> Some Local.root
    | Some p -> Some (Local.of_string (if is_dir_sep p.[0] then String.drop p 1 else p))
  ;;

  let try_localize_external ext =
    let p = External.to_string ext in
    match Fdecl.get Build.build_dir with
    | External s ->
      (match drop_absolute_prefix ~prefix:(External s) p with
       | Some s -> Some (in_build_dir s)
       | None ->
         drop_absolute_prefix ~prefix:(In_source_dir Local.root) p
         |> Option.map ~f:in_source_tree)
    | In_source_dir _ ->
      drop_absolute_prefix ~prefix:(In_source_dir Local.root) p
      |> Option.map ~f:make_local_path
  ;;

  let try_localize_external t =
    match t with
    | External e -> Option.value ~default:t (try_localize_external e)
    | _ -> t
  ;;
end
