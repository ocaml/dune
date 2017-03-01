open! Import

module Dir = struct
  type t =
    { path     : Path.t
    ; files    : String_set.t
    ; sub_dirs : t String_map.t
    }

  let path t = t.path
  let files t = t.files
  let sub_dirs t = t.sub_dirs

  let rec fold t ~init ~f =
    let init = f t init in
    String_map.fold t.sub_dirs ~init ~f:(fun ~key:_ ~data:t acc ->
        fold t ~init:acc ~f)
end

type t =
  { root : Dir.t
  ; dirs : Dir.t Path.Map.t
  }

let root t = t.root

let ignore_file = function
  | ""
  | "_build"
  | ".git"
  | ".hg"
  | "_darcs"
  | "." -> true
  | fn -> fn.[0] = '.' && fn.[1] = '#'

let load path =
  let rec walk path : Dir.t =
    let files, sub_dirs =
      Path.readdir path
      |> List.filter ~f:(fun fn ->
          not (ignore_file fn))
      |> List.partition_map ~f:(fun fn ->
          let path = Path.relative path fn in
          if Path.exists path && Path.is_directory path then
            Inr (fn, walk path)
          else
            Inl fn)
    in
    { path
    ; files    = String_set.of_list files
    ; sub_dirs = String_map.of_alist_exn sub_dirs
    }
  in
  let root = walk path in
  let dirs =
    Dir.fold root ~init:Path.Map.empty ~f:(fun dir acc ->
      Path.Map.add acc ~key:dir.path ~data:dir)
  in
  { root
  ; dirs
  }

let fold t ~init ~f =
  Path.Map.fold t.dirs ~init ~f:(fun ~key:_ ~data:dir acc -> f dir acc)

let find_dir t path =
  Path.Map.find path t.dirs

let file_exists t path fn =
  match Path.Map.find path t.dirs with
  | None -> false
  | Some { files; _ } -> String_set.mem fn files

let exists t path =
  Path.Map.mem path t.dirs ||
  file_exists t (Path.parent path) (Path.basename path)
