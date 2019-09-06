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

  let of_stats (st : Unix.stats) = { ino = st.st_ino; dev = st.st_dev }

  module Map = Map.Make (struct
    type nonrec t = t

    let compare = compare

    let to_dyn _ = Dyn.opaque
  end)

  let of_source_path p = of_stats (Path.stat (Path.source p))
end

module Dune_file = struct
  module Plain = struct
    type t =
      { path : Path.Source.t
      ; mutable sexps : Dune_lang.Ast.t list
      }
  end

  type t =
    | Plain of Plain.t
    | Ocaml_script of Path.Source.t

  let path = function
    | Plain x -> x.path
    | Ocaml_script p -> p

  let load file ~project =
    Io.with_lexbuf_from_file (Path.source file) ~f:(fun lb ->
        let t, sub_dirs =
          if Dune_lexer.is_script lb then
            (Ocaml_script file, Sub_dirs.default)
          else
            let sexps = Dune_lang.Parser.parse lb ~mode:Many in
            let decoder =
              Dune_project.set_parsing_context project Sub_dirs.decode
            in
            let sub_dirs, sexps =
              Dune_lang.Decoder.parse decoder Univ_map.empty
                (Dune_lang.Ast.List (Loc.none, sexps))
            in
            (Plain { path = file; sexps }, sub_dirs)
        in
        (t, sub_dirs))
end

module Dir = struct
  type t =
    { path : Path.Source.t
    ; status : Sub_dirs.Status.t
    ; contents : contents Lazy.t
    ; project : Dune_project.t
    ; vcs : Vcs.t option
    }

  and contents =
    { files : String.Set.t
    ; sub_dirs : t String.Map.t
    ; dune_file : Dune_file.t option
    }

  let create ~project ~path ~status ~contents ~vcs =
    { path; status; contents; project; vcs }

  let contents t = Lazy.force t.contents

  let path t = t.path

  let ignored t = t.status = Data_only

  let vendored t = t.status = Vendored

  let files t = (contents t).files

  let sub_dirs t = (contents t).sub_dirs

  let dune_file t = (contents t).dune_file

  let project t = t.project

  let vcs t = t.vcs

  let file_paths t =
    Path.Source.Set.of_listing ~dir:t.path
      ~filenames:(String.Set.to_list (files t))

  let sub_dir_names t =
    String.Map.foldi (sub_dirs t) ~init:String.Set.empty ~f:(fun s _ acc ->
        String.Set.add acc s)

  let sub_dir_paths t =
    String.Map.foldi (sub_dirs t) ~init:Path.Source.Set.empty
      ~f:(fun s _ acc ->
        Path.Source.Set.add acc (Path.Source.relative t.path s))

  let rec fold t ~traverse ~init:acc ~f =
    let must_traverse = Sub_dirs.Status.Map.find traverse t.status in
    if must_traverse then
      let acc = f t acc in
      String.Map.fold (sub_dirs t) ~init:acc ~f:(fun t acc ->
          fold t ~traverse ~init:acc ~f)
    else
      acc

  let rec dyn_of_contents { files; sub_dirs; dune_file } =
    let open Dyn in
    Record
      [ ("files", String.Set.to_dyn files)
      ; ("sub_dirs", String.Map.to_dyn to_dyn sub_dirs)
      ; ("dune_file", Dyn.Encoder.(option opaque dune_file))
      ; ("project", Dyn.opaque)
      ]

  and to_dyn { path; status; contents = (lazy contents); project = _; vcs } =
    let open Dyn in
    Record
      [ ("path", Path.Source.to_dyn path)
      ; ("status", Sub_dirs.Status.to_dyn status)
      ; ("contents", dyn_of_contents contents)
      ; ("vcs", Dyn.Encoder.option Vcs.to_dyn vcs)
      ]
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
    User_warning.emit
      [ Pp.textf "Unable to read directory %s. Ignoring."
          (Path.Source.to_string_maybe_quoted path)
      ; Pp.text "Remove this message by ignoring by adding:"
      ; Pp.textf "(dirs \\ %s)" (Path.Source.basename path)
      ; Pp.textf "to the dune file: %s"
          (Path.Source.to_string_maybe_quoted
             (Path.Source.relative (Path.Source.parent_exn path) "dune"))
      ; Pp.textf "Reason: %s" (Unix.error_message unix_error)
      ];
    Error unix_error
  | Ok unsorted_contents ->
    let files, dirs =
      List.filter_partition_map unsorted_contents ~f:(fun fn ->
          let path = Path.Source.relative path fn in
          if Path.Source.is_in_build_dir path then
            Skip
          else
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
              Left fn)
    in
    { files = String.Set.of_list files
    ; dirs =
        List.sort dirs ~compare:(fun (a, _, _) (b, _, _) -> String.compare a b)
    }
    |> Result.ok

let load path ~ancestor_vcs ~recognize_jbuilder_projects =
  let open Result.O in
  let nb_path_visited = ref 0 in
  let rec walk path ~dirs_visited ~project:parent_project ~vcs
      ~(dir_status : Sub_dirs.Status.t) { dirs; files } =
    incr nb_path_visited;
    if !nb_path_visited mod 100 = 0 then
      Console.update_status_line
        (Pp.verbatim (Printf.sprintf "Scanned %i directories" !nb_path_visited));
    let project =
      if dir_status = Data_only then
        parent_project
      else
        Option.value
          (Dune_project.load ~dir:path ~files
             ~infer_from_opam_files:recognize_jbuilder_projects)
          ~default:parent_project
    in
    let vcs =
      match
        match
          List.find_map dirs ~f:(function
            | ".git", _, _ -> Some Vcs.Kind.Git
            | ".hg", _, _ -> Some Vcs.Kind.Hg
            | _ -> None)
        with
        | Some kind -> Some kind
        | None -> Vcs.Kind.of_dir_contents files
      with
      | Some kind -> Some { Vcs.kind; root = Path.(append_source root) path }
      | None -> vcs
    in
    let contents =
      lazy
        (let dune_file, sub_dirs =
           if dir_status = Data_only then
             (None, Sub_dirs.default)
           else (
             if
               (not recognize_jbuilder_projects)
               && String.Set.mem files "jbuild"
             then
               User_error.raise
                 ~loc:
                   (Loc.in_file
                      (Path.source (Path.Source.relative path "jbuild")))
                 [ Pp.text
                     "jbuild files are no longer supported, please convert \
                      this file to a dune file instead."
                 ; Pp.text
                     "Note: You can use \"dune upgrade\" to convert your \
                      project to dune."
                 ];
             if not (String.Set.mem files "dune") then
               (None, Sub_dirs.default)
             else (
               ignore
                 ( Dune_project.ensure_project_file_exists project
                   : Dune_project.created_or_already_exist );
               let file = Path.Source.relative path "dune" in
               let dune_file, sub_dirs = Dune_file.load file ~project in
               (Some dune_file, sub_dirs)
             )
           )
         in
         let sub_dirs =
           Sub_dirs.eval sub_dirs ~dirs:(List.map ~f:(fun (a, _, _) -> a) dirs)
         in
         let sub_dirs =
           dirs
           |> List.fold_left ~init:String.Map.empty
                ~f:(fun acc (fn, path, file) ->
                  let status =
                    if Bootstrap.data_only_path path then
                      Sub_dirs.Status.Or_ignored.Ignored
                    else
                      Sub_dirs.status sub_dirs ~dir:fn
                  in
                  match status with
                  | Ignored -> acc
                  | Status status -> (
                    let dir_status : Sub_dirs.Status.t =
                      match (dir_status, status) with
                      | Data_only, _ -> Data_only
                      | Vendored, Normal -> Vendored
                      | _, _ -> status
                    in
                    let dirs_visited =
                      if Sys.win32 then
                        dirs_visited
                      else
                        match File.Map.find dirs_visited file with
                        | None -> File.Map.set dirs_visited file path
                        | Some first_path ->
                          User_error.raise
                            [ Pp.textf
                                "Path %s has already been scanned. Cannot \
                                 scan it again through symlink %s"
                                (Path.Source.to_string_maybe_quoted first_path)
                                (Path.Source.to_string_maybe_quoted path)
                            ]
                    in
                    match
                      let+ x = readdir path in
                      walk path ~dirs_visited ~project ~dir_status ~vcs x
                    with
                    | Ok dir -> String.Map.set acc fn dir
                    | Error _ -> acc ))
         in
         { Dir.files; sub_dirs; dune_file })
    in
    Dir.create ~path ~contents ~status:dir_status ~project ~vcs
  in
  let walk =
    let+ x = readdir path in
    let project =
      match
        Dune_project.load ~dir:path ~files:x.files ~infer_from_opam_files:true
      with
      | None -> Dune_project.anonymous ~dir:path
      | Some p -> p
    in
    walk path
      ~dirs_visited:(File.Map.singleton (File.of_source_path path) path)
      ~dir_status:Normal ~project ~vcs:ancestor_vcs x
  in
  Console.clear_status_line ();
  match walk with
  | Ok dir -> dir
  | Error m ->
    User_error.raise
      [ Pp.textf "Unable to load source %s.@.Reason:%s@."
          (Path.Source.to_string_maybe_quoted path)
          (Unix.error_message m)
      ]

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

let rec nearest_dir t = function
  | [] -> t
  | comp :: components -> (
    match String.Map.find (Dir.sub_dirs t) comp with
    | None -> t
    | Some t -> nearest_dir t components )

let nearest_dir t path =
  let components = Path.Source.explode path in
  nearest_dir t components

let nearest_vcs t path = Dir.vcs (nearest_dir t path)

let files_of t path =
  match find_dir t path with
  | None -> Path.Source.Set.empty
  | Some dir ->
    Path.Source.Set.of_list
      (List.map
         (String.Set.to_list (Dir.files dir))
         ~f:(Path.Source.relative path))

let file_exists t path =
  match find_dir t (Path.Source.parent_exn path) with
  | None -> false
  | Some dir -> String.Set.mem (Dir.files dir) (Path.Source.basename path)

let dir_exists t path = Option.is_some (find_dir t path)

let dir_is_vendored t path =
  Option.map ~f:(fun dir -> Dir.vendored dir) (find_dir t path)
