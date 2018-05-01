open! Import

module Dir = struct
  type t =
    { path     : Path.t
    ; ignored  : bool
    ; contents : contents Lazy.t
    }

  and contents =
    { files    : String.Set.t
    ; sub_dirs : t String.Map.t
    }

  let contents t = Lazy.force t.contents

  let path t = t.path
  let ignored t = t.ignored

  let files    t = (contents t).files
  let sub_dirs t = (contents t).sub_dirs

  let file_paths t =
    Path.Set.of_string_set (files t) ~f:(Path.relative t.path)

  let sub_dir_names t =
    String.Map.foldi (sub_dirs t) ~init:String.Set.empty
      ~f:(fun s _ acc -> String.Set.add acc s)

  let sub_dir_paths t =
    String.Map.foldi (sub_dirs t) ~init:Path.Set.empty
      ~f:(fun s _ acc -> Path.Set.add acc (Path.relative t.path s))

  let rec fold t ~traverse_ignored_dirs ~init:acc ~f =
    if not traverse_ignored_dirs && t.ignored then
      acc
    else
      let acc = f t acc in
      String.Map.fold (sub_dirs t) ~init:acc ~f:(fun t acc ->
        fold t ~traverse_ignored_dirs ~init:acc ~f)
end

type t =
  { root : Dir.t
  ; dirs : (Path.t, Dir.t) Hashtbl.t
  }

let root t = t.root

let ignore_file fn ~is_directory =
  fn = "" || fn = "." ||
  (is_directory && (fn.[0] = '.' || fn.[0] = '_')) ||
  (fn.[0] = '.' && fn.[1] = '#')

let load ?(extra_ignored_subtrees=Path.Set.empty) path =
  let rec walk path ~ignored : Dir.t =
    let contents = lazy (
      let files, sub_dirs =
        Path.readdir path
        |> List.filter_partition_map ~f:(fun fn ->
          let path = Path.relative path fn in
          let is_directory = Path.is_directory path in
          if ignore_file fn ~is_directory then
            Skip
          else if is_directory then
            Right (fn, path)
          else
            Left fn)
      in
      let files = String.Set.of_list files in
      let ignored_sub_dirs =
        if not ignored && String.Set.mem files "jbuild-ignore" then
          let ignore_file = Path.relative path "jbuild-ignore" in
          let files =
            Io.lines_of_file ignore_file
          in
          let remove_subdirs index fn =
            if Filename.dirname fn = Filename.current_dir_name then
              true
            else begin
              Loc.(warn (of_pos ( Path.to_string ignore_file
                                , index + 1, 0, String.length fn))
                     "subdirectory expression %s ignored" fn);
              false
            end
          in
          String.Set.of_list (List.filteri ~f:remove_subdirs files)
        else
          String.Set.empty
      in
      let sub_dirs =
        List.map sub_dirs ~f:(fun (fn, path) ->
          let ignored =
            ignored
            || String.Set.mem ignored_sub_dirs fn
            || Path.Set.mem extra_ignored_subtrees path
          in
          (fn, walk path ~ignored))
        |> String.Map.of_list_exn
      in
      { Dir. files; sub_dirs })
    in
    { path
    ; contents
    ; ignored
    }
  in
  let root = walk path ~ignored:false in
  let dirs = Hashtbl.create 1024      in
  Hashtbl.add dirs Path.root root;
  { root; dirs }

let fold t ~traverse_ignored_dirs ~init ~f =
  Dir.fold t.root ~traverse_ignored_dirs ~init ~f

let rec find_dir t path =
  if not (Path.is_local path) then
    None
  else
    match Hashtbl.find t.dirs path with
    | Some _ as res -> res
    | None ->
      match
        let open Option.O in
        find_dir t (Path.parent path)
        >>= fun parent ->
        String.Map.find (Dir.sub_dirs parent) (Path.basename path)
      with
      | Some dir as res ->
        Hashtbl.add t.dirs path dir;
        res
      | None ->
        (* We don't cache failures in [t.dirs]. The expectation is
           that these only happen when the user writes an invalid path
           in a jbuild file, so there is no need to cache them. *)
        None

let files_of t path =
  match find_dir t path with
  | None -> Path.Set.empty
  | Some dir ->
    Path.Set.of_string_set (Dir.files dir) ~f:(Path.relative path)

let file_exists t path fn =
  match find_dir t path with
  | None -> false
  | Some dir -> String.Set.mem (Dir.files dir) fn

let dir_exists t path = Option.is_some (find_dir t path)

let exists t path =
  dir_exists t path ||
  file_exists t (Path.parent path) (Path.basename path)

let files_recursively_in t ?(prefix_with=Path.root) path =
  match find_dir t path with
  | None -> Path.Set.empty
  | Some dir ->
    Dir.fold dir ~init:Path.Set.empty ~traverse_ignored_dirs:true
      ~f:(fun dir acc ->
        let path = Path.append prefix_with (Dir.path dir) in
        String.Set.fold (Dir.files dir) ~init:acc ~f:(fun fn acc ->
          Path.Set.add acc (Path.relative path fn)))
