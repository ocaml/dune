open! Stdune
open Import

module Dune_file = struct
  module Plain = struct
    type t =
      { path          : Path.t
      ; mutable sexps : Dune_lang.Ast.t list
      }
  end

  module Contents = struct
    type t =
      | Plain of Plain.t
      | Ocaml_script of Path.t
  end

  type t =
    { contents : Contents.t
    ; kind     : Dune_lang.Syntax.t
    }

  let path t =
    match t.contents with
    | Plain         x -> x.path
    | Ocaml_script  p -> p

  let load file ~project ~kind =
    Io.with_lexbuf_from_file file ~f:(fun lb ->
      let contents, sub_dirs =
        if Dune_lexer.is_script lb then
          (Contents.Ocaml_script file, Sub_dirs.default)
        else
          let sexps =
            Dune_lang.Parser.parse lb
              ~lexer:(Dune_lang.Lexer.of_syntax kind) ~mode:Many
          in
          let decoder =
            Dune_project.set_parsing_context project Sub_dirs.decode in
          let sub_dirs, sexps =
            Dune_lang.Decoder.parse decoder
              Univ_map.empty (Dune_lang.Ast.List (Loc.none, sexps)) in
          (Plain { path = file; sexps }, sub_dirs)
      in
      ({ contents; kind }, sub_dirs))
end

let load_jbuild_ignore path =
  List.filteri (Io.lines_of_file path) ~f:(fun i fn ->
    if Filename.dirname fn = Filename.current_dir_name then
      true
    else begin
      Errors.(warn (Loc.of_pos
                      ( Path.to_string path
                      , i + 1, 0
                      , String.length fn
                      ))
                "subdirectory expression %s ignored" fn);
      false
    end)
  |> String.Set.of_list

module Dir = struct
  type t =
    { path     : Path.t
    ; ignored  : bool
    ; contents : contents Lazy.t
    }

  and contents =
    { files     : String.Set.t
    ; sub_dirs  : t String.Map.t
    ; dune_file : Dune_file.t option
    ; project   : Dune_project.t
    }

  let contents t = Lazy.force t.contents

  let path t = t.path
  let ignored t = t.ignored

  let files     t = (contents t).files
  let sub_dirs  t = (contents t).sub_dirs
  let dune_file t = (contents t).dune_file
  let project   t = (contents t).project

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

type t = Dir.t

let root t = t

module File = struct
  type t =
    { ino : int
    ; dev : int
    }

  let compare a b =
    match Int.compare a.ino b.ino with
    | Eq -> Int.compare a.dev b.dev
    | ne -> ne

  let dummy = { ino = 0; dev = 0 }

  let of_stats (st : Unix.stats) =
    { ino = st.st_ino
    ; dev = st.st_dev
    }

  module Map = Map.Make(struct type nonrec t = t let compare = compare end)
end

let is_temp_file fn =
  String.is_prefix fn ~prefix:".#"
  || String.is_suffix fn ~suffix:".swp"
  || String.is_suffix fn ~suffix:"~"

let load ?(warn_when_seeing_jbuild_file=true) path =
  let rec walk path ~dirs_visited ~project ~data_only : Dir.t =
    let contents = lazy (
      let files, unfiltered_sub_dirs =
        Path.readdir_unsorted path
        |> List.filter_partition_map ~f:(fun fn ->
          let path = Path.relative path fn in
          if Path.is_in_build_dir path then
            Skip
          else begin
            let is_directory, file =
              match Unix.stat (Path.to_string path) with
              | exception _ -> (false, File.dummy)
              | { st_kind = S_DIR; _ } as st ->
                (true, File.of_stats st)
              | _ ->
                (false, File.dummy)
            in
            if is_directory then
              Right (fn, path, file)
            else if is_temp_file fn then
              Skip
            else
              Left fn
          end)
      in
      let files = String.Set.of_list files in
      let unfiltered_sub_dirs =
        List.sort
          ~compare:(fun (a, _, _) (b, _, _) -> String.compare a b)
          unfiltered_sub_dirs
      in
      let project, dune_file, sub_dirs =
        if data_only then
          (project, None, Sub_dirs.default)
        else
          let project =
            Option.value (Dune_project.load ~dir:path ~files) ~default:project
          in
          let dune_file, sub_dirs =
            match List.filter ["dune"; "jbuild"] ~f:(String.Set.mem files) with
            | [] -> (None, Sub_dirs.default)
            | [fn] ->
              let file = Path.relative path fn in
              if fn = "dune" then
                ignore (Dune_project.ensure_project_file_exists project
                        : Dune_project.created_or_already_exist)
              else if warn_when_seeing_jbuild_file then
                Errors.warn (Loc.in_file file)
                  "jbuild files are deprecated, please convert this file to \
                   a dune file instead.\n\
                   Note: You can use \"dune upgrade\" to convert your \
                   project to dune.";
              let dune_file, sub_dirs =
                Dune_file.load file
                  ~project
                  ~kind:(Option.value_exn (Dune_lang.Syntax.of_basename fn))
              in
              (Some dune_file, sub_dirs)
            | _ ->
              die "Directory %s has both a 'dune' and 'jbuild' file.\n\
                   This is not allowed"
                (Path.to_string_maybe_quoted path)
          in
          let sub_dirs =
            if String.Set.mem files "jbuild-ignore" then
              Sub_dirs.add_data_only_dirs sub_dirs
                ~dirs:(load_jbuild_ignore (Path.relative path "jbuild-ignore"))
            else
              sub_dirs
          in
          (project, dune_file, sub_dirs)
      in
      let sub_dirs =
        Sub_dirs.eval sub_dirs
          ~dirs:(List.map ~f:(fun (a, _, _) -> a) unfiltered_sub_dirs) in
      let sub_dirs =
        List.fold_left unfiltered_sub_dirs ~init:String.Map.empty
          ~f:(fun acc (fn, path, file) ->
            let status =
              if Bootstrap.data_only_path path then
                Sub_dirs.Status.Ignored
              else
                Sub_dirs.status sub_dirs ~dir:fn
            in
            match status with
            | Ignored -> acc
            | Normal | Data_only ->
              let data_only = data_only || status = Data_only in
              let dirs_visited =
                if Sys.win32 then
                  dirs_visited
                else
                  match File.Map.find dirs_visited file with
                  | None -> File.Map.add dirs_visited file path
                  | Some first_path ->
                    die "Path %s has already been scanned. \
                         Cannot scan it again through symlink %s"
                      (Path.to_string_maybe_quoted first_path)
                      (Path.to_string_maybe_quoted path)
              in
              walk path ~dirs_visited ~project ~data_only
              |> String.Map.add acc fn)
      in
      { Dir. files; sub_dirs; dune_file; project })
    in
    { path
    ; contents
    ; ignored = data_only
    }
  in
  walk path
    ~dirs_visited:(File.Map.singleton
                     (File.of_stats (Unix.stat (Path.to_string path)))
                     path)
    ~data_only:false
    ~project:(Lazy.force Dune_project.anonymous)

let fold = Dir.fold

let rec find_dir t path =
  if not (Path.is_managed path) then
    None
  else if Path.equal (Dir.path t) path then
    Some t
  else
    match
      let open Option.O in
      Path.parent path
      >>= find_dir t
      >>= fun parent ->
      String.Map.find (Dir.sub_dirs parent) (Path.basename path)
    with
    | Some _ as res -> res
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
  file_exists t (Path.parent_exn path) (Path.basename path)

let files_recursively_in t ?(prefix_with=Path.root) path =
  match find_dir t path with
  | None -> Path.Set.empty
  | Some dir ->
    Dir.fold dir ~init:Path.Set.empty ~traverse_ignored_dirs:true
      ~f:(fun dir acc ->
        let path = Path.append prefix_with (Dir.path dir) in
        String.Set.fold (Dir.files dir) ~init:acc ~f:(fun fn acc ->
          Path.Set.add acc (Path.relative path fn)))
