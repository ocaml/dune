open! Stdune
open Import

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

  let of_source_path p = of_stats (Path.stat (Path.source p))
end

module Dune_file = struct
  module Plain = struct
    type t =
      { path          : Path.Source.t
      ; mutable sexps : Dune_lang.Ast.t list
      }
  end

  module Contents = struct
    type t =
      | Plain of Plain.t
      | Ocaml_script of Path.Source.t
  end

  type t =
    { contents : Contents.t
    ; kind     : Dune_lang.File_syntax.t
    }

  let path t =
    match t.contents with
    | Plain         x -> x.path
    | Ocaml_script  p -> p

  let load file ~project ~kind =
    Io.with_lexbuf_from_file (Path.source file) ~f:(fun lb ->
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
    { path     : Path.Source.t
    ; ignored  : bool
    ; contents : contents Lazy.t
    ; project  : Dune_project.t
    ; vcs      : Vcs.t option
    }

  and contents =
    { files     : String.Set.t
    ; sub_dirs  : t String.Map.t
    ; dune_file : Dune_file.t option
    }

  let create ~project ~path ~ignored ~contents ~vcs =
    { path
    ; ignored
    ; contents
    ; project
    ; vcs
    }

  let contents t = Lazy.force t.contents

  let path t = t.path
  let ignored t = t.ignored

  let files     t = (contents t).files
  let sub_dirs  t = (contents t).sub_dirs
  let dune_file t = (contents t).dune_file

  let project t = t.project
  let vcs t = t.vcs

  let file_paths t =
    Path.Source.Set.of_list
      (List.map (String.Set.to_list (files t)) ~f:(Path.Source.relative_exn t.path))

  let sub_dir_names t =
    String.Map.foldi (sub_dirs t) ~init:String.Set.empty
      ~f:(fun s _ acc -> String.Set.add acc s)

  let sub_dir_paths t =
    String.Map.foldi (sub_dirs t) ~init:Path.Source.Set.empty
      ~f:(fun s _ acc -> Path.Source.Set.add acc (Path.Source.relative_exn t.path s))

  let rec fold t ~traverse_ignored_dirs ~init:acc ~f =
    if not traverse_ignored_dirs && t.ignored then
      acc
    else
      let acc = f t acc in
      String.Map.fold (sub_dirs t) ~init:acc ~f:(fun t acc ->
        fold t ~traverse_ignored_dirs ~init:acc ~f)

  let rec dyn_of_contents { files; sub_dirs; dune_file } =
    let open Dyn in
    Record
      [ "files", String.Set.to_dyn files
      ; "sub_dirs", String.Map.to_dyn to_dyn sub_dirs
      ; "dune_file", Dyn.Encoder.(option opaque dune_file)
      ; "project", Dyn.opaque
      ]

  and to_dyn { path ; ignored ; contents = lazy contents ; project = _; vcs } =
    let open Dyn in
    Record
      [ "path", Path.Source.to_dyn path
      ; "ignored", Bool ignored
      ; "contents", dyn_of_contents contents
      ; "vcs", Dyn.Encoder.option Vcs.to_dyn vcs
      ]

  let to_sexp t = Dyn.to_sexp (to_dyn t)
end

type t = Dir.t

let root t = t

let is_temp_file fn =
  String.is_prefix fn ~prefix:".#"
  || String.is_suffix fn ~suffix:".swp"
  || String.is_suffix fn ~suffix:"~"

type readdir =
  { files : String.Set.t
  ; dirs : (string * Path.Source.t * File.t) list
  }

let readdir path =
  match Path.readdir_unsorted (Path.source path) with
  | Error unix_error ->
    Errors.warn Loc.none
      "Unable to read directory %s. Ignoring.@.\
       Remove this message by ignoring by adding:@.\
       (dirs \\ %s)@.\
       to the dune file: %s@.\
       Reason: %s@."
      (Path.Source.to_string_maybe_quoted path)
      (Path.Source.basename path)
      (Path.Source.to_string_maybe_quoted
         (Path.Source.relative_exn (Path.Source.parent_exn path) "dune"))
      (Unix.error_message unix_error);
    Error unix_error
  | Ok unsorted_contents ->
    let files, dirs =
      List.filter_partition_map unsorted_contents ~f:(fun fn ->
        let path = Path.Source.relative_exn path fn in
        if Path.Source.is_in_build_dir path then
          Skip
        else begin
          let is_directory, file =
            match Path.stat (Path.source path) with
            | exception _ -> (false, File.dummy)
            | { st_kind = S_DIR; _ } as st -> (true, File.of_stats st)
            | _ -> (false, File.dummy)
          in
          if is_directory then
            Right (fn, path, file)
          else if is_temp_file fn then
            Skip
          else
            Left fn
        end)
    in
    { files = String.Set.of_list files
    ; dirs =
        List.sort dirs
          ~compare:(fun (a, _, _) (b, _, _) -> String.compare a b)
    }
    |> Result.ok

let load ?(warn_when_seeing_jbuild_file=true) path ~ancestor_vcs =
  let open Result.O in
  let rec walk path ~dirs_visited ~project:parent_project ~vcs ~data_only
    : (_, _) Result.t =
    let+ { dirs; files } = readdir path in
    let project =
      if data_only then
        parent_project
      else
        Option.value (Dune_project.load ~dir:path ~files)
          ~default:parent_project
    in
    let vcs =
      match
        match
          List.find_map dirs ~f:(function
            | (".git", _, _) -> Some Vcs.Kind.Git
            | (".hg", _, _) -> Some Vcs.Kind.Hg
            | _ -> None)
        with
        | Some kind -> Some kind
        | None -> Vcs.Kind.of_dir_contents files
      with
      | Some kind -> Some { Vcs.kind; root = Path.(append_source root) path }
      | None -> vcs
    in
    let contents = lazy (
      let dune_file, sub_dirs =
        if data_only then
          (None, Sub_dirs.default)
        else
          let dune_file, sub_dirs =
            match List.filter ["dune"; "jbuild"] ~f:(String.Set.mem files) with
            | [] -> (None, Sub_dirs.default)
            | [fn] ->
              let file = Path.Source.relative_exn path fn in
              if fn = "dune" then
                ignore (Dune_project.ensure_project_file_exists project
                        : Dune_project.created_or_already_exist)
              else if warn_when_seeing_jbuild_file then
                Errors.warn (Loc.in_file (Path.source file))
                  "jbuild files are deprecated, please convert this file to \
                   a dune file instead.\n\
                   Note: You can use \"dune upgrade\" to convert your \
                   project to dune.";
              let dune_file, sub_dirs =
                Dune_file.load file
                  ~project
                  ~kind:(Option.value_exn
                           (Dune_lang.File_syntax.of_basename fn))
              in
              (Some dune_file, sub_dirs)
            | _ ->
              die "Directory %s has both a 'dune' and 'jbuild' file.\n\
                   This is not allowed"
                (Path.Source.to_string_maybe_quoted path)
          in
          let sub_dirs =
            if String.Set.mem files "jbuild-ignore" then
              Sub_dirs.add_data_only_dirs sub_dirs
                ~dirs:(load_jbuild_ignore (
                  Path.source (Path.Source.relative_exn path "jbuild-ignore")))
            else
              sub_dirs
          in
          (dune_file, sub_dirs)
      in
      let sub_dirs =
        Sub_dirs.eval sub_dirs
          ~dirs:(List.map ~f:(fun (a, _, _) -> a) dirs) in
      let sub_dirs =
        dirs
        |> List.fold_left ~init:String.Map.empty ~f:(fun acc (fn, path, file) ->
          let status =
            if Bootstrap.data_only_path (Path.source path) then
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
                    (Path.Source.to_string_maybe_quoted first_path)
                    (Path.Source.to_string_maybe_quoted path)
            in
            match
              walk path ~dirs_visited ~project ~data_only ~vcs
            with
            | Ok dir -> String.Map.add acc fn dir
            | Error _ -> acc)
      in
      { Dir. files; sub_dirs; dune_file })
    in
    Dir.create ~path ~contents ~ignored:data_only ~project ~vcs
  in
  match
    walk path
      ~dirs_visited:(File.Map.singleton (File.of_source_path path) path)
      ~data_only:false
      ~project:(Lazy.force Dune_project.anonymous)
      ~vcs:ancestor_vcs
  with
  | Ok dir -> dir
  | Error m ->
    die "Unable to load source %s.@.Reason:%s@."
      (Path.Source.to_string_maybe_quoted path) (Unix.error_message m)

let fold = Dir.fold

let rec find_dir t = function
  | [] -> Some t
  | comp :: components ->
    let open Option.O in
    let* t = String.Map.find (Dir.sub_dirs t) comp in
    find_dir t components

let find_dir t path =
  let components = Path.Source.explode path in
  find_dir t components

let files_of t path =
  match find_dir t path with
  | None -> Path.Source.Set.empty
  | Some dir ->
    Path.Source.Set.of_list (
      List.map (String.Set.to_list (Dir.files dir)) ~f:(Path.Source.relative_exn path))

let file_exists t path fn =
  match find_dir t path with
  | None -> false
  | Some dir -> String.Set.mem (Dir.files dir) fn

let dir_exists t path = Option.is_some (find_dir t path)

let exists t path =
  dir_exists t path ||
  file_exists t (Path.Source.parent_exn path) (Path.Source.basename path)

let files_recursively_in t ?(prefix_with=Path.root) path =
  match find_dir t path with
  | None -> Path.Set.empty
  | Some dir ->
    Dir.fold dir ~init:Path.Set.empty ~traverse_ignored_dirs:true
      ~f:(fun dir acc ->
        let path = Path.append_source prefix_with (Dir.path dir) in
        String.Set.fold (Dir.files dir) ~init:acc ~f:(fun fn acc ->
          Path.Set.add acc (Path.relative_exn path fn)))
