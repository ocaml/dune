open! Import

module Dune_fs = struct
  open Sexp.Of_sexp

  type kind =
    | File
    | Dir
    | Any

  type what =
    | Standard
    | Raw_data
    | Ignore

  type entry =
    { kind : kind
    ; glob : Re.re
    ; what : what
    }

  type t = { rev_entries : entry list } [@@unboxed]

  let find =
    let rec aux rev_entries fn ~is_directory =
      match rev_entries with
      | [] ->
        if fn = "" || fn = "." ||
           (is_directory && (fn.[0] = '.' || fn.[0] = '_')) ||
           (fn.[0] = '.' && fn.[1] = '#')
        then
          Ignore
        else
          Standard
      | e :: rest ->
        if (match e.kind with
          | File -> not is_directory
          | Dir  -> is_directory
          | Any  -> true) &&
           Re.execp e.glob fn then
          e.what
        else
          aux rest fn ~is_directory
    in
    fun t fn ~is_directory ->
      aux t.rev_entries fn ~is_directory

  let kind =
    enum
      [ "file" , File
      ; "dir"  , Dir
      ; "_"    , Any
      ]

  let what =
    enum
      [ "standard" , Standard
      ; "raw_data" , Raw_data
      ; "ignore"   , Ignore
      ]

  let glob sexp =
    let s = string sexp in
    match Glob_lexer.parse_string s with
    | Ok re -> Re.compile re
    | Error (_pos, msg) -> of_sexp_errorf sexp "invalid glob: %s" msg

  let entry sexp =
    let kind, glob, what = triple kind glob what sexp in
    { kind; glob; what }

  let t sexps =
    let rev_entries = List.rev_map sexps ~f:entry in
    { rev_entries }

  let load path =
    t (Io.Sexp.load path ~mode:Many)

  let load_jbuild_ignore path =
    let rev_entries =
      List.rev_filter_mapi (Io.lines_of_file path) ~f:(fun i fn ->
        if Filename.dirname fn = Filename.current_dir_name then
          Some { kind = Dir; glob = Re.compile (Re.str fn); what = Raw_data }
        else begin
          Loc.(warn (of_pos ( Path.to_string path
                            , i + 1, 0
                            , String.length fn
                            ))
                 "subdirectory expression %s ignored" fn);
          None
        end)
    in
    { rev_entries }

  let default = { rev_entries = [] }
end

module Dir = struct
  type t =
    { path     : Path.t
    ; raw_data : bool
    ; contents : contents Lazy.t
    }

  and contents =
    { files    : String.Set.t
    ; sub_dirs : t String.Map.t
    ; dune_fs  : Dune_fs.t
    }

  let contents t = Lazy.force t.contents

  let path t = t.path
  let raw_data t = t.raw_data

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

  let rec fold t ~traverse_raw_data_dirs ~init:acc ~f =
    if not traverse_raw_data_dirs && t.raw_data then
      acc
    else
      let acc = f t acc in
      String.Map.fold (sub_dirs t) ~init:acc ~f:(fun t acc ->
        fold t ~traverse_raw_data_dirs ~init:acc ~f)

  let dune_file t =
    let (lazy { files; dune_fs; _ }) = t.contents in
    List.find_map ["dune"; "jbuild"] ~f:(fun fn ->
      if String.Set.mem files fn then
        match Dune_fs.find dune_fs fn ~is_directory:false with
        | Standard -> Some (Path.relative t.path fn)
        | _        -> None
      else
        None)
end

type t =
  { root : Dir.t
  ; dirs : (Path.t, Dir.t) Hashtbl.t
  }

let root t = t.root

let load ?(extra_ignored_subtrees=Path.Set.empty) path =
  let rec walk path ~raw_data : Dir.t =
    let contents = lazy (
      let files, sub_dirs =
        Path.readdir path
        |> List.partition_map ~f:(fun fn ->
          let path = Path.relative path fn in
          if Path.is_directory path then
            Right (fn, path)
          else
            Left fn)
      in
      let files = String.Set.of_list files in
      let dune_fs =
        if String.Set.mem files ".dune-fs" then
          Dune_fs.load (Path.relative path ".dune-fs")
        else if String.Set.mem files "jbuild-ignore" then
          Dune_fs.load_jbuild_ignore (Path.relative path "jbuild-ignore")
        else
          Dune_fs.default
      in
      let files =
        String.Set.filter files ~f:(fun fn ->
          match Dune_fs.find dune_fs fn ~is_directory:false with
          | Ignore              -> false
          | Standard | Raw_data -> true)
      in
      let sub_dirs =
        List.fold_left sub_dirs ~init:String.Map.empty ~f:(fun acc (fn, path) ->
          match
            match Dune_fs.find dune_fs fn ~is_directory:true with
            | Ignore   -> None
            | Standard -> Some (raw_data ||
                                Path.Set.mem extra_ignored_subtrees path)
            | Raw_data -> Some true
          with
          | None -> acc
          | Some raw_data -> String.Map.add acc fn (walk path ~raw_data))
      in
      { Dir. files; sub_dirs; dune_fs })
    in
    { path
    ; contents
    ; raw_data
    }
  in
  let root = walk path ~raw_data:false in
  let dirs = Hashtbl.create 1024      in
  Hashtbl.add dirs Path.root root;
  { root; dirs }

let fold t ~traverse_raw_data_dirs ~init ~f =
  Dir.fold t.root ~traverse_raw_data_dirs ~init ~f

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
    Dir.fold dir ~init:Path.Set.empty ~traverse_raw_data_dirs:true
      ~f:(fun dir acc ->
        let path = Path.append prefix_with (Dir.path dir) in
        String.Set.fold (Dir.files dir) ~init:acc ~f:(fun fn acc ->
          Path.Set.add acc (Path.relative path fn)))
