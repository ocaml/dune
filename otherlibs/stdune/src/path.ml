module Sys = Stdlib.Sys

let basename_opt ~is_root ~basename t = if is_root t then None else Some (basename t)

let is_dir_sep =
  if Sys.win32 || Sys.cygwin
  then fun c -> c = '/' || c = '\\' || c = ':'
  else fun c -> c = '/'
;;

let explode_path =
  let rec start acc path i =
    if i < 0
    then acc
    else if is_dir_sep (String.unsafe_get path i)
    then start acc path (i - 1)
    else component acc path i (i - 1)
  and component acc path end_ i =
    if i < 0
    then String.take path (end_ + 1) :: acc
    else if is_dir_sep (String.unsafe_get path i)
    then start (String.sub path ~pos:(i + 1) ~len:(end_ - i) :: acc) path (i - 1)
    else component acc path end_ (i - 1)
  in
  fun path ->
    if path = Filename.current_dir_name
    then [ path ]
    else (
      match start [] path (String.length path - 1) with
      | "." :: xs -> xs
      | xs -> xs)
;;

let append_with_slash x y =
  let len_x = String.length x in
  let len_y = String.length y in
  let dst = Bytes.create (len_x + 1 + len_y) in
  Bytes.blit_string ~src:x ~src_pos:0 ~dst ~dst_pos:0 ~len:len_x;
  Bytes.set dst len_x '/';
  Bytes.blit_string ~src:y ~src_pos:0 ~dst ~dst_pos:(len_x + 1) ~len:len_y;
  Bytes.unsafe_to_string dst
;;

module Unspecified = Path_intf.Unspecified

module Local_gen : sig
  include Path_intf.Local_gen

  module Prefix : sig
      type 'w local = 'w t
      type 'w t

      val make : 'w local -> 'w t
      val drop : 'w t -> 'w local -> 'w local option

      (* for all local path p, drop (invalid p = None) *)
      val invalid : 'w t
    end
    with type 'w local := 'w t
end = struct
  (* either "." for root, or a '/' separated list of components other that ".",
     ".." and not containing '/'. *)
  type _ t = string

  module Table = String.Table

  let to_string t = t
  let hash = String.hash
  let compare = String.compare
  let equal = String.equal
  let root = "."
  let is_root t = Ordering.is_eq (compare t root)

  let parent t =
    if is_root t
    then None
    else (
      match String.rindex_from t (String.length t - 1) '/' with
      | None -> Some root
      | Some i -> Some (String.take t i))
  ;;

  let unlink_no_err t = Fpath.unlink_no_err t

  let basename t =
    if is_root t
    then Code_error.raise "Path.Local.basename called on the root" []
    else (
      let len = String.length t in
      match String.rindex_from t (len - 1) '/' with
      | None -> t
      | Some i -> String.sub t ~pos:(i + 1) ~len:(len - i - 1))
  ;;

  let to_dyn t = Dyn.String t

  module L = struct
    let relative_result t components =
      let rec loop t components =
        match components with
        | [] -> Result.Ok t
        | "." :: rest -> loop t rest
        | ".." :: rest ->
          (match parent t with
           | None -> Result.Error `Outside_the_workspace
           | Some parent -> loop parent rest)
        | fn :: rest ->
          if is_root t then loop fn rest else loop (append_with_slash t fn) rest
      in
      loop t components
    ;;

    let relative ?error_loc t components =
      match relative_result t components with
      | Result.Ok t -> t
      | Error `Outside_the_workspace ->
        User_error.raise
          ?loc:error_loc
          [ Pp.textf
              "path outside the workspace: %s from %s"
              (String.concat ~sep:"/" components)
              t
          ]
    ;;
  end

  let relative ?error_loc t path =
    if not (Filename.is_relative path)
    then
      Code_error.raise
        "Local.relative: received absolute path"
        [ "t", to_dyn t; "path", String path ];
    match L.relative_result t (explode_path path) with
    | Result.Ok t -> t
    | Error `Outside_the_workspace ->
      User_error.raise
        ?loc:error_loc
        [ Pp.textf "path outside the workspace: %s from %s" path t ]
  ;;

  (* Check whether a path is in canonical form: no '.' or '..' components, no
     repeated '/' components, no backslashes '\\' (on Windows only), and not
     ending in a slash '/'. *)

  let is_canonicalized =
    let rec before_slash s i =
      if i < 0
      then false
      else (
        match s.[i] with
        | '/' -> false
        | '.' -> before_dot_slash s (i - 1)
        | '\\' when Sys.win32 -> false
        | _ -> in_component s (i - 1))
    and before_dot_slash s i =
      if i < 0
      then false
      else (
        match s.[i] with
        | '/' -> false
        | '.' -> before_dot_dot_slash s (i - 1)
        | '\\' when Sys.win32 -> false
        | _ -> in_component s (i - 1))
    and before_dot_dot_slash s i =
      if i < 0
      then false
      else (
        match s.[i] with
        | '/' -> false
        | '\\' when Sys.win32 -> false
        | _ -> in_component s (i - 1))
    and in_component s i =
      if i < 0
      then true
      else (
        match s.[i] with
        | '/' -> before_slash s (i - 1)
        | '\\' when Sys.win32 -> false
        | _ -> in_component s (i - 1))
    in
    fun s ->
      let len = String.length s in
      len = 0 || before_slash s (len - 1)
  ;;

  let parse_string_exn ~loc s =
    match s with
    | "" | "." -> root
    | _ when is_canonicalized s -> s
    | _ -> relative root s ~error_loc:loc
  ;;

  let of_string s = parse_string_exn ~loc:Loc0.none s

  let append a b =
    match is_root a, is_root b with
    | true, _ -> b
    | _, true -> a
    | _, _ -> append_with_slash a b
  ;;

  let descendant t ~of_ =
    if is_root of_
    then Some t
    else if t = of_
    then Some root
    else (
      let of_len = String.length of_ in
      let t_len = String.length t in
      if t_len > of_len && t.[of_len] = '/' && String.is_prefix t ~prefix:of_
      then Some (String.drop t (of_len + 1))
      else None)
  ;;

  let is_descendant t ~of_ =
    is_root of_
    || t = of_
    ||
    let of_len = String.length of_ in
    let t_len = String.length t in
    t_len > of_len && t.[of_len] = '/' && String.is_prefix t ~prefix:of_
  ;;

  module Reach = struct
    (* count the number of times we need to do ".." *)
    let parent_remaining_components pos from =
      let len = String.length from in
      if pos >= len
      then 0
      else (
        let count = ref 1 in
        let pos = if Char.equal from.[pos] '/' then pos + 1 else pos in
        for i = pos to len - 1 do
          if Char.equal from.[i] '/' then incr count
        done;
        !count)
    ;;

    (* generate a sequence of ".." separated by "/" [times] in [buf] *)
    let gen_blit_go_up buf ~times =
      if times > 0
      then (
        String_builder.add_string buf "..";
        for _ = 1 to times - 1 do
          String_builder.add_string buf "/.."
        done)
    ;;

    (* because the ".." above are so common, we precompute the first 20 cases *)
    let blit_go_up_table =
      Array.init 20 ~f:(fun i ->
        List.init (i + 1) ~f:(fun _ -> "..") |> String.concat ~sep:"/")
    ;;

    let blit_go_up buf ~times =
      if times > 0
      then
        if times > Array.length blit_go_up_table
        then (* doing the work in a single blit is fastest *)
          gen_blit_go_up buf ~times
        else (
          let src = blit_go_up_table.(times - 1) in
          String_builder.add_string buf src)
    ;;

    (* the size of the "../.." string we need to generate *)
    let go_up_components_buffer_size times = (times * 2) + max 0 (times - 1)

    let reach_root ~from pos =
      let go_up_this_many_times = parent_remaining_components pos from in
      if go_up_this_many_times = 0
      then "."
      else if go_up_this_many_times <= Array.length blit_go_up_table
      then blit_go_up_table.(go_up_this_many_times - 1)
      else (
        let size = go_up_components_buffer_size go_up_this_many_times in
        let buf = String_builder.create size in
        blit_go_up buf ~times:go_up_this_many_times;
        String_builder.build_exact_exn buf [@nontail])
    ;;

    (* if we have "a/b" and "a", we need to skip over the "a", even if the last
       component position is [0] *)
    let extend_to_comp ~smaller ~bigger ~pos ~comp =
      if pos = String.length smaller && bigger.[pos] = '/' then pos else comp
    ;;

    let make_from_common_prefix ~to_ ~from to_pos =
      let to_len = String.length to_ in
      let to_pos = if to_pos < to_len && to_.[to_pos] = '/' then to_pos + 1 else to_pos in
      let to_len = to_len - to_pos in
      let go_up_this_many_times = parent_remaining_components to_pos from in
      if to_len = 0
      then reach_root ~from to_pos
      else (
        let size = go_up_components_buffer_size go_up_this_many_times in
        let add_extra_slash = size > 0 && to_len > 0 in
        (* the final length of the buffer we need to compute *)
        let size = to_len + size + if add_extra_slash then 1 else 0 in
        (* our position inside the buffer *)
        let buf = String_builder.create size in
        blit_go_up buf ~times:go_up_this_many_times;
        if add_extra_slash then String_builder.add_char buf '/';
        String_builder.add_substring buf to_ ~pos:to_pos ~len:to_len;
        String_builder.build_exact_exn buf [@nontail])
    ;;

    let rec common_prefix ~to_ ~from ~pos ~comp =
      if Int.equal pos (String.length to_)
      then (
        (* the case where we exhausted [to_] first. *)
        let pos = extend_to_comp ~smaller:to_ ~bigger:from ~pos ~comp in
        make_from_common_prefix ~to_ ~from pos)
      else if Int.equal pos (String.length from)
      then (
        (* we exhausted [from] first *)
        let pos = extend_to_comp ~smaller:from ~bigger:to_ ~pos ~comp in
        make_from_common_prefix ~to_ ~from pos)
      else if Char.equal to_.[pos] from.[pos]
      then (
        (* eat another common character. *)
        let comp =
          (* if we find '/', then we advance the last common component position *)
          if to_.[pos] = '/' then pos else comp
        in
        common_prefix ~to_ ~from ~pos:(pos + 1) ~comp)
      else make_from_common_prefix ~to_ ~from comp
    ;;

    let reach to_ ~from =
      if is_root from
      then to_
      else if is_root to_
      then reach_root ~from 0
      else if equal to_ from
      then "."
      else common_prefix ~to_ ~from ~pos:0 ~comp:0
    ;;
  end

  let reach = Reach.reach
  let extend_basename t ~suffix = t ^ suffix
  let extension t = Filename.extension t

  let split_extension t =
    let s, ext = Filename.split_extension t in
    s, ext
  ;;

  let set_extension t ~ext =
    let base, _ = split_extension t in
    base ^ ext
  ;;

  let map_extension t ~f =
    let base, ext = split_extension t in
    base ^ f ext
  ;;

  module Prefix = struct
    type _ t =
      { len : int
      ; path : string
      ; path_slash : string
      }

    let make p =
      if is_root p then Code_error.raise "Path.Local.Prefix.make" [ "path", to_dyn p ];
      { len = String.length p; path = p; path_slash = p ^ "/" }
    ;;

    let drop t p =
      let len = String.length p in
      if len = t.len && p = t.path
      then Some root
      else String.drop_prefix p ~prefix:t.path_slash
    ;;

    let invalid = { len = -1; path = "/"; path_slash = "/" }
  end

  let split_first_component t =
    if is_root t
    then None
    else (
      match String.lsplit2 t ~on:'/' with
      | None -> Some (t, root)
      | Some (before, after) -> Some (before, after |> of_string))
  ;;

  let explode p = if is_root p then [] else String.split p ~on:'/'
  let to_string_maybe_quoted t = String.maybe_quoted t

  let parent_exn t =
    match parent t with
    | None -> Code_error.raise "Path.Local.parent:exn t is root" [ "t", to_dyn t ]
    | Some parent -> parent
  ;;

  module Fix_root (Root : sig
      type w
    end) =
  struct
    type _w = Root.w

    module Table = Table
    module Map = String.Map

    module Set = struct
      include String.Set

      let of_listing ~dir ~filenames = of_list_map filenames ~f:(fun f -> relative dir f)
    end
  end
end

module Local : sig
  type w = Unspecified.w
  type t = w Local_gen.t

  include Path_intf.S with type t := t
  module Table : Hashtbl.S with type key = t

  val root : t
  val is_root : t -> bool
  val relative : ?error_loc:Loc0.t -> t -> string -> t
  val append : t -> t -> t
  val descendant : t -> of_:t -> t option
  val is_descendant : t -> of_:t -> bool
  val reach : t -> from:t -> string

  module L : sig
    val relative : ?error_loc:Loc0.t -> t -> string list -> t
    val relative_result : t -> string list -> (t, [ `Outside_the_workspace ]) Result.t
  end

  val split_first_component : t -> (string * t) option
  val explode : t -> string list
  val of_local : t -> t

  module Prefix : sig
      type local = t
      type t

      val make : local -> t
      val drop : t -> local -> local option

      (* for all local path p, drop (invalid p = None) *)
      val invalid : t
    end
    with type local := t
end = struct
  type w = Unspecified.w

  include (
    Local_gen :
      module type of Local_gen
      with type 'a t := 'a Local_gen.t
      with module Prefix := Local_gen.Prefix)

  type nonrec t = w Local_gen.t

  module Prefix = struct
    open Local_gen
    include (Prefix : module type of Prefix with type 'a t := 'a Prefix.t)

    type t = w Prefix.t
  end

  include (
    Comparator.Operators (struct
      type nonrec t = t

      let compare = Local_gen.compare
    end) :
      Comparator.OPS with type t := t)

  let of_local t = t

  include Fix_root (struct
      type nonrec w = w
    end)

  let basename_opt = basename_opt ~is_root ~basename
end

module External : sig
  include Path_intf.S
  module Table : Hashtbl.S with type key = t

  val relative : t -> string -> t
  val mkdir_p : ?perms:int -> t -> unit
  val initial_cwd : t
  val cwd : unit -> t
  val as_local : t -> string
  val append_local : t -> Local.t -> t
  val of_filename_relative_to_initial_cwd : string -> t
end = struct
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

  let mkdir_p ?perms path = ignore (Fpath.mkdir_p ?perms path : Fpath.mkdir_p_result)
  let unlink_no_err t = Fpath.unlink_no_err t
  let extension t = Filename.extension t

  let split_extension t =
    let s, ext = Filename.split_extension t in
    s, ext
  ;;

  let set_extension t ~ext =
    let base, _ = split_extension t in
    base ^ ext
  ;;

  let map_extension t ~f =
    let base, ext = split_extension t in
    base ^ f ext
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
    if is_root a then true else String.is_prefix ~prefix:(to_string a ^ "/") (to_string b)
  ;;

  module Map = String.Map

  module Set = struct
    include String.Set

    let of_listing ~dir ~filenames = of_list_map filenames ~f:(fun f -> relative dir f)
  end
end

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
    | External t -> External.mkdir_p ?perms t
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
  let chmod t ~mode = Unix.chmod (to_string t) mode
  let lstat t = Unix.lstat (to_string t)
  let unlink t = Fpath.unlink (to_string t)
  let unlink_no_err t = Fpath.unlink_no_err (to_string t)
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

let is_strict_descendant_of_build_dir = function
  | In_build_dir p -> not (Local.is_root p)
  | In_source_tree _ | External _ -> false
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

let as_outside_build_dir_exn : t -> Outside_build_dir.t = function
  | In_source_tree s -> In_source_dir s
  | External s -> External s
  | In_build_dir path ->
    Code_error.raise "as_outside_build_dir_exn" [ "path", Build.to_dyn path ]
;;

let destruct_build_dir : t -> [ `Inside of Build.t | `Outside of Outside_build_dir.t ]
  = function
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

let explode t =
  match local_or_external t with
  | In_source_dir p when Local.is_root p -> Some []
  | In_source_dir s -> Some (String.split (Local.to_string s) ~on:'/')
  | External _ -> None
;;

let explode_exn t =
  match explode t with
  | Some s -> s
  | None -> Code_error.raise "Path.explode_exn" [ "path", to_dyn t ]
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

let exists t =
  try Sys.file_exists (to_string t) with
  | Sys_error _ -> false
;;

let readdir_unsorted t = Dune_filesystem_stubs.read_directory (to_string t)

let readdir_unsorted_with_kinds t =
  Dune_filesystem_stubs.read_directory_with_kinds (to_string t)
;;

let is_directory t =
  try Sys.is_directory (to_string t) with
  | Sys_error _ -> false
;;

let rmdir t = Unix.rmdir (to_string t)
let unlink_exn t = Fpath.unlink_exn (to_string t)

let link src dst =
  match Unix.link (to_string src) (to_string dst) with
  | exception Unix.Unix_error (Unix.EUNKNOWNERR -1142, syscall, arg)
  (* Needed for OCaml < 5.1 on windows *) ->
    Exn.reraise (Unix.Unix_error (Unix.EMLINK, syscall, arg))
  | s -> s
;;

let unlink_no_err t = Fpath.unlink_no_err (to_string t)
let build_dir_exists () = is_directory build_dir

let ensure_build_dir_exists () =
  let perms = 0o777 in
  match local_or_external build_dir with
  | In_source_dir p -> Relative_to_source_root.mkdir_p p ~perms
  | External p ->
    let p = External.to_string p in
    (match Fpath.mkdir ~perms p with
     | Created | Already_exists -> ()
     | Missing_parent_directory ->
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

let clear_dir dir = Fpath.clear_dir (to_string dir)

let rm_rf ?(allow_external = false) t =
  if (not allow_external) && not (is_managed t)
  then Code_error.raise "Path.rm_rf called on external dir" [ "t", to_dyn t ];
  Fpath.rm_rf (to_string t)
;;

let mkdir_p ?perms = function
  | External s -> External.mkdir_p s ?perms
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
  extend_basename ~suffix:(f ext) base
;;

module O = Comparable.Make (T)
module Map = O.Map

module Set = struct
  include O.Set

  let of_listing ~dir ~filenames = of_list_map filenames ~f:(fun f -> relative dir f)
end

let in_source s = in_source_tree (Local.of_string s)
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
    Source0.Table.filteri_inplace source ~f:(fun [@inline] ~key ~data ->
      f ~key:(In_source_tree key) ~data);
    Build.Table.filteri_inplace build ~f:(fun [@inline] ~key ~data ->
      f ~key:(In_build_dir key) ~data);
    External.Table.filteri_inplace external_ ~f:(fun [@inline] ~key ~data ->
      f ~key:(External key) ~data)
  ;;

  let filter_inplace { source; build; external_ } ~f =
    Source0.Table.filteri_inplace source ~f:(fun [@inline] ~key:_ ~data -> f data);
    Build.Table.filteri_inplace build ~f:(fun [@inline] ~key:_ ~data -> f data);
    External.Table.filteri_inplace external_ ~f:(fun [@inline] ~key:_ ~data -> f data)
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

let stat_exn t = Unix.stat (to_string t)
let stat t = Dune_filesystem_stubs.Unix_error.Detailed.catch stat_exn t
let lstat_exn t = Unix.lstat (to_string t)
let lstat t = Dune_filesystem_stubs.Unix_error.Detailed.catch lstat_exn t

include (Comparator.Operators (T) : Comparator.OPS with type t := t)

let path_of_local = of_local

module Source = struct
  include Source0

  let is_in_build_dir s = is_in_build_dir (path_of_local s)
  let to_local t = t
end

let set_of_source_paths set = Source.Set.to_list set |> Set.of_list_map ~f:source

let set_of_build_paths_list =
  List.fold_left ~init:Set.empty ~f:(fun acc e -> Set.add acc (build e))
;;

let set_of_external_paths set = External.Set.to_list set |> Set.of_list_map ~f:external_
let rename old_path new_path = Unix.rename (to_string old_path) (to_string new_path)
let chmod t ~mode = Unix.chmod (to_string t) mode
let follow_symlink path = Fpath.follow_symlink (to_string path) |> Result.map ~f:of_string

let drop_prefix path ~prefix =
  if prefix = path
  then Some Local.root
  else (
    let prefix_s = to_string prefix in
    let prefix =
      if String.is_suffix ~suffix:"/" prefix_s then prefix_s else prefix_s ^ "/"
    in
    let open Option.O in
    let+ suffix = String.drop_prefix (to_string path) ~prefix in
    Local.of_string suffix)
;;

let drop_prefix_exn t ~prefix =
  match drop_prefix t ~prefix with
  | None ->
    Code_error.raise "Path.drop_prefix_exn" [ "t", to_dyn t; "prefix", to_dyn prefix ]
  | Some p -> p
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
