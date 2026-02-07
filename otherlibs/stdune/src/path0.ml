module Unspecified = Path_intf.Unspecified

open struct
  let append_with_slash x y =
    let len_x = String.length x in
    let len_y = String.length y in
    let dst = Bytes.create (len_x + 1 + len_y) in
    Bytes.blit_string ~src:x ~src_pos:0 ~dst ~dst_pos:0 ~len:len_x;
    Bytes.set dst len_x '/';
    Bytes.blit_string ~src:y ~src_pos:0 ~dst ~dst_pos:(len_x + 1) ~len:len_y;
    Bytes.unsafe_to_string dst
  ;;
end

let is_dir_sep =
  if Sys.win32 || Sys.cygwin
  then fun c -> c = '/' || c = '\\' || c = ':'
  else fun c -> c = '/'
;;

let basename_opt ~is_root ~basename t = if is_root t then None else Some (basename t)

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

module Local_gen = struct
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
      if t_len > of_len && t.[of_len] = '/' && String.starts_with ~prefix:of_ t
      then Some (String.drop t (of_len + 1))
      else None)
  ;;

  let is_descendant t ~of_ =
    is_root of_
    || t = of_
    ||
    let of_len = String.length of_ in
    let t_len = String.length t in
    t_len > of_len && t.[of_len] = '/' && String.starts_with ~prefix:of_ t
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
  let split_extension t = Filename.split_extension t

  let set_extension t ~ext =
    let base, _ = split_extension t in
    base ^ Filename.Extension.to_string ext
  ;;

  let map_extension t ~f =
    let base, ext = split_extension t in
    base ^ Filename.Extension.Or_empty.to_string (f ext)
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

module Local = struct
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
