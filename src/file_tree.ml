open! Import

module Dir = struct
  type t =
    { path     : Path.t
    ; files    : String.Set.t
    ; sub_dirs : t String.Map.t
    ; ignored  : bool
    }

  let path t = t.path
  let files t = t.files
  let sub_dirs t = t.sub_dirs
  let ignored t = t.ignored

  let file_paths t =
    Path.Set.of_string_set t.files ~f:(Path.relative t.path)

  let sub_dir_names t =
    String.Map.foldi t.sub_dirs ~init:String.Set.empty
      ~f:(fun s _ acc -> String.Set.add acc s)

  let sub_dir_paths t =
    String.Map.foldi t.sub_dirs ~init:Path.Set.empty
      ~f:(fun s _ acc -> Path.Set.add acc (Path.relative t.path s))

  let rec fold t ~traverse_ignored_dirs ~init:acc ~f =
    if not traverse_ignored_dirs && t.ignored then
      acc
    else
      let acc = f t acc in
      String.Map.fold t.sub_dirs ~init:acc ~f:(fun t acc ->
        fold t ~traverse_ignored_dirs ~init:acc ~f)
end

type t =
  { root : Dir.t
  ; dirs : Dir.t Path.Map.t
  }

let root t = t.root

let ignore_file fn ~is_directory =
  fn = "" || fn = "." ||
  (is_directory && (fn.[0] = '.' || fn.[0] = '_')) ||
  (fn.[0] = '.' && fn.[1] = '#')

let load ?(extra_ignored_subtrees=Path.Set.empty) path =
  let rec walk path ~ignored : Dir.t =
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
    { path
    ; files
    ; sub_dirs
    ; ignored
    }
  in
  let root = walk path ~ignored:false in
  let dirs =
    Dir.fold root ~init:Path.Map.empty ~traverse_ignored_dirs:true
      ~f:(fun dir acc ->
        Path.Map.add acc dir.path dir)
  in
  { root
  ; dirs
  }

let fold t ~traverse_ignored_dirs ~init ~f =
  Dir.fold t.root ~traverse_ignored_dirs ~init ~f

let find_dir t path = Path.Map.find t.dirs path

let files_of t path =
  match find_dir t path with
  | None -> Path.Set.empty
  | Some dir ->
    Path.Set.of_string_set (Dir.files dir) ~f:(Path.relative path)

let file_exists t path fn =
  match Path.Map.find t.dirs path with
  | None -> false
  | Some { files; _ } -> String.Set.mem files fn

let exists t path =
  Path.Map.mem t.dirs path ||
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
