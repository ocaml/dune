open Import
open Memo.O

module Dirs_visited : sig
  (** Unique set of all directories visited *)
  type t

  val singleton : Path.Source.t -> Dir_contents.File.t -> t
  val empty : t
  val add : t -> Path.Source.t -> Dir_contents.File.t -> t
end = struct
  type t = Path.Source.t Dir_contents.File.Map.t

  let empty = Dir_contents.File.Map.empty
  let singleton path file = Dir_contents.File.Map.singleton file path

  let add (t : t) (path : Path.Source.t) file =
    if Sys.win32
    then t
    else
      Dir_contents.File.Map.update t file ~f:(function
        | None -> Some path
        | Some first_path ->
          User_error.raise
            [ Pp.textf
                "Path %s has already been scanned. Cannot scan it again through symlink \
                 %s"
                (Path.Source.to_string_maybe_quoted first_path)
                (Path.Source.to_string_maybe_quoted path)
            ])
  ;;
end

module Dir0 = struct
  module Vcs = struct
    type nonrec t =
      | This of Vcs.t
      | Ancestor_vcs

    let get_vcs ~default:vcs ~readdir ~path =
      match
        Vcs.Kind.of_dir_contents
          ~files:(Dir_contents.files readdir)
          ~dirs:(Dir_contents.dirs readdir)
      with
      | None -> vcs
      | Some kind -> This { Vcs.kind; root = Path.(append_source root) path }
    ;;
  end

  type t =
    { path : Path.Source.t
    ; status : Source_dir_status.t
    ; files : Filename.Array.Set.t
    ; sub_dirs : sub_dir Filename.Array.Map.t
    ; dune_file : Dune_file.t option
    ; project : Dune_project.t
    ; vcs : Vcs.t
    }

  and sub_dir =
    { sub_dir_status : Source_dir_status.t
    ; sub_dir_as_t : t Memo.t
    }

  let rec to_dyn { path; status; files; dune_file; sub_dirs; vcs = _; project = _ } =
    Dyn.record
      [ "path", Path.Source.to_dyn path
      ; "status", Source_dir_status.to_dyn status
      ; "files", Dyn.Set (Filename.Array.Set.to_list_map files ~f:Filename.to_dyn)
      ; ( "sub_dirs"
        , Dyn.Map
            (Filename.Array.Map.to_list_map sub_dirs ~f:(fun name sub_dir ->
               Filename.to_dyn name, dyn_of_sub_dir sub_dir)) )
      ; ("dune_file", Dyn.(option opaque dune_file))
      ]

  and dyn_of_sub_dir { sub_dir_status; sub_dir_as_t = _ } =
    Dyn.record [ "status", Source_dir_status.to_dyn sub_dir_status ]
  ;;

  let path t = t.path
  let status t = t.status
  let filenames t = t.files
  let sub_dirs t = t.sub_dirs
  let dune_file t = t.dune_file
  let project t = t.project
  let sub_dir_names t = Filename.Array.Map.keys t.sub_dirs
  let sub_dir_as_t (s : sub_dir) = s.sub_dir_as_t
end

let eval_status ~status_map ~(parent_status : Source_dir_status.t) dir
  : Source_dir_status.t option
  =
  match Source_dir_status.Per_dir.status status_map ~dir with
  | Ignored -> None
  | Status status ->
    Some
      (match parent_status, status with
       | Data_only, _ -> Data_only
       | Vendored, Normal -> Vendored
       | _, _ -> status)
;;

let error_unable_to_load ~path unix_error =
  User_error.raise
    [ Pp.textf "Unable to load source %s." (Path.Source.to_string_maybe_quoted path)
    ; Unix_error.Detailed.pp_reason unix_error
    ]
;;

let rec physical
          ~project
          ~default_vcs
          ~dir
          ~dirs_visited
          ~dirs
          ~sub_dirs
          ~dune_file
          ~parent_status
  =
  let status_map =
    Source_dir_status.Spec.eval sub_dirs ~dirs:(Filename.Array.Map.keys dirs)
  in
  Filename.Array.Map.filter_mapi dirs ~f:(fun fn file ->
    match eval_status ~status_map ~parent_status fn with
    | None -> None
    | Some dir_status ->
      let path = Path.Source.relative_fname dir fn in
      let dirs_visited = Dirs_visited.add dirs_visited path file in
      Some
        { Dir0.sub_dir_status = dir_status
        ; sub_dir_as_t =
            Memo.lazy_node ~name:"source-tree-sub-dir" (fun () ->
              find_dir_raw
                ~default_vcs
                ~path
                ~basename:fn
                ~virtual_:false
                ~dirs_visited
                ~dune_file
                ~status:dir_status
                ~project)
            |> Memo.Node.read
        })

and virtual_ ~project ~sub_dirs ~parent_status ~dune_file ~init ~path =
  match dune_file with
  | None -> init
  | Some df ->
    (* There's no files to read for virtual directories, but we still record
       their entries *)
    let dirs = Dune_file.sub_dirnames df in
    let status_map = Source_dir_status.Spec.eval sub_dirs ~dirs in
    let virtual_dirs =
      Filename.Array.Set.to_list_map dirs ~f:(fun fn ->
        match eval_status ~status_map ~parent_status fn with
        | None -> None
        | Some status ->
          if Filename.Array.Map.mem init fn
          then None
          else
            Some
              ( fn
              , { Dir0.sub_dir_status = status
                ; sub_dir_as_t =
                    Memo.lazy_node ~name:"source-tree-virtual-sub-dir" (fun () ->
                      find_dir_raw
                        ~default_vcs:Dir0.Vcs.Ancestor_vcs
                        ~path:(Path.Source.relative_fname path fn)
                        ~basename:fn
                        ~virtual_:true
                        ~dune_file
                        ~status
                        ~dirs_visited:Dirs_visited.empty
                        ~project)
                    |> Memo.Node.read
                } ))
      |> List.filter_opt
      |> Filename.Array.Map.of_sorted_list_exn
    in
    Filename.Array.Map.union_left_biased init virtual_dirs

and contents
      readdir
      ~default_vcs
      ~path
      ~dune_file
      ~dirs_visited
      ~project
      ~(dir_status : Source_dir_status.t)
  =
  let files = Dir_contents.files readdir in
  let+ dune_file =
    Dune_file.load
      ~dir:(Source_tree_file.Dir.workspace path)
      dir_status
      project
      ~files
      ~parent:dune_file
  in
  let files =
    let predicate =
      match dune_file with
      | None -> Dune_file.Files.default
      | Some dune_file -> Dune_file.files dune_file
    in
    Dune_file.Files.eval predicate ~files
  in
  let vcs = Dir0.Vcs.get_vcs ~default:default_vcs ~readdir ~path in
  let sub_dirs =
    let sub_dirs =
      match dune_file with
      | None -> Source_dir_status.Spec.default
      | Some dune_file -> Dune_file.sub_dir_status dune_file
    in
    let dirs =
      physical
        ~default_vcs:vcs
        ~project
        ~dir:path
        ~dirs_visited
        ~dirs:(Dir_contents.dirs readdir)
        ~sub_dirs
        ~dune_file
        ~parent_status:dir_status
    in
    virtual_ ~project ~sub_dirs ~parent_status:dir_status ~dune_file ~path ~init:dirs
  in
  { Dir0.project; vcs; status = dir_status; path; files; sub_dirs; dune_file }

and find_dir_raw
      ~virtual_
      ~default_vcs
      ~dune_file
      ~status
      ~dirs_visited
      ~project
      ~path
      ~basename
  : Dir0.t Memo.t
  =
  let status =
    if Dune_project.cram project && Cram_test.is_cram_suffix basename
    then Source_dir_status.Data_only
    else status
  in
  let* readdir =
    if virtual_
    then Memo.return Dir_contents.empty
    else
      Dir_contents.of_source_path path
      >>| function
      | Ok dir -> dir
      | Error _ -> Dir_contents.empty
  in
  let* project =
    if status = Data_only
    then Memo.return project
    else
      Dune_project.load
        ~dir:path
        ~files:(Dir_contents.files readdir)
        ~infer_from_opam_files:false
        ~load_opam_file_with_contents:Dune_pkg.Opam_file.load_opam_file_with_contents
      >>| Option.map
            ~f:(Only_packages.filter_packages_in_project ~vendored:(status = Vendored))
      >>| Option.value ~default:project
  in
  contents readdir ~default_vcs ~path ~dune_file ~dirs_visited ~project ~dir_status:status
;;

let root =
  Memo.lazy_node ~name:"source-tree-root"
  @@ fun () ->
  let path = Path.Source.root in
  let dir_status : Source_dir_status.t = Normal in
  let* readdir =
    Dir_contents.of_source_path path
    >>| function
    | Ok dir -> dir
    | Error unix_error -> error_unable_to_load ~path unix_error
  in
  let vcs = Dir0.Vcs.get_vcs ~default:Ancestor_vcs ~readdir ~path in
  let* project =
    Dune_project.load
      ~dir:path
      ~files:(Dir_contents.files readdir)
      ~infer_from_opam_files:true
      ~load_opam_file_with_contents:Dune_pkg.Opam_file.load_opam_file_with_contents
    >>| (function
     | Some p -> p
     | None ->
       Dune_project.anonymous
         ~dir:(Source_path.workspace path)
         Package_info.empty
         Package.Name.Map.empty)
    >>| Only_packages.filter_packages_in_project ~vendored:(dir_status = Vendored)
  in
  let* dirs_visited =
    Dir_contents.File.of_source_path path
    >>| function
    | Ok file -> Dirs_visited.singleton path file
    | Error unix_error -> error_unable_to_load ~path unix_error
  in
  contents
    readdir
    ~default_vcs:vcs
    ~path
    ~dune_file:None
    ~dirs_visited
    ~project
    ~dir_status
;;

let gen_find_dir =
  let rec loop on_success on_last_found components (dir : Dir0.t) =
    match components with
    | [] -> on_success dir
    | x :: xs ->
      (match Filename.Array.Map.find dir.sub_dirs x with
       | None -> on_last_found dir
       | Some dir -> dir.sub_dir_as_t >>= loop on_success on_last_found xs)
  in
  fun ~on_success ~on_last_found p ->
    Memo.Node.read root >>= loop on_success on_last_found (Path.Source.explode p)
;;

let find_dir =
  gen_find_dir
    ~on_success:(fun dir -> Memo.return (Some dir))
    ~on_last_found:(fun _ -> Memo.return None)
;;

let nearest_dir = gen_find_dir ~on_success:Memo.return ~on_last_found:Memo.return

let find_excluded_ancestor path =
  let rec loop (dir : Dir0.t) = function
    | [] -> Memo.return None
    | sub_dir :: path ->
      (match Filename.Array.Map.find dir.sub_dirs sub_dir with
       | Some sub_dir ->
         let* child = sub_dir.sub_dir_as_t in
         loop child path
       | None ->
         Dir_contents.of_source_path dir.path
         >>| (function
          | Ok contents when Filename.Array.Map.mem (Dir_contents.dirs contents) sub_dir
            ->
            dir.dune_file
            |> Option.bind ~f:Dune_file.dirs_stanza_loc
            |> Option.map ~f:(fun loc -> Path.Source.relative_fname dir.path sub_dir, loc)
          | _ -> None))
  in
  let* root = Memo.Node.read root in
  loop root (Path.Source.explode path)
;;

let root () = Memo.Node.read root

let files_of path =
  find_dir path
  >>| function
  | None -> Path.Source.Set.empty
  | Some dir ->
    Dir0.filenames dir
    |> Filename.Array.Set.to_list
    |> Path.Source.Set.of_list_map ~f:(Path.Source.relative_fname path)
;;

module Dir = struct
  include Dir0

  module Make_map_reduce (M : Memo.S) (Outcome : Monoid) = struct
    open M.O

    let map_reduce =
      let rec map_reduce t ~traverse ~trace_event_name ~f =
        let must_traverse = Source_dir_status.Map.find traverse t.status in
        match must_traverse with
        | false -> M.return Outcome.empty
        | true ->
          let+ here = f t
          and+ in_sub_dirs =
            M.List.map
              (Filename.Array.Map.to_list_map t.sub_dirs ~f:(fun _ s -> s))
              ~f:(fun s ->
                let* t = M.of_memo (sub_dir_as_t s) in
                map_reduce t ~traverse ~trace_event_name ~f)
          in
          List.fold_left in_sub_dirs ~init:here ~f:Outcome.combine
      in
      let impl =
        lazy
          (match Dune_trace.global () with
           | None -> map_reduce
           | Some trace ->
             fun t ~traverse ~trace_event_name ~f ->
               let start = Time.now () in
               let+ res = map_reduce t ~traverse ~trace_event_name ~f in
               let stop = Time.now () in
               let event =
                 Dune_trace.Event.scan_source
                   ~name:trace_event_name
                   ~start
                   ~stop
                   ~dir:t.path
               in
               Dune_trace.Out.emit trace event;
               res)
      in
      fun t ~traverse ~trace_event_name ~f ->
        (Lazy.force impl) t ~traverse ~trace_event_name ~f
    ;;
  end
end

module Workspace_dir = Dir

module Rules = struct
  module File = struct
    include Source_tree_file.File

    let include_context = Include_stanza.in_source_file
  end

  module Dir = struct
    type loaded =
      { logical_path : Path.Build.t
      ; backing_path : Path.Build.t
      ; layers : Source_tree_file.Mounted_layer.t list
      ; relative_dir : Path.Local.t
      ; status : Source_dir_status.t
      ; files : Filename.Array.Set.t
      ; sub_dirs : loaded_sub_dir Filename.Array.Map.t
      ; dune_file : Dune_file.t option
      ; project : Dune_project.t
      }

    and loaded_sub_dir = { dir : loaded Memo.t }

    type t =
      | Source of Workspace_dir.t
      | Mounted of loaded

    type sub_dir =
      | Source_sub_dir of Workspace_dir.sub_dir
      | Mounted_sub_dir of loaded_sub_dir

    let source dir = Source dir

    let make_loaded
          ~logical_path
          ~backing_path
          ~layers
          ~relative_dir
          ~status
          ~files
          ~sub_dirs
          ~dune_file
          ~project
      =
      { logical_path
      ; backing_path
      ; layers
      ; relative_dir
      ; status
      ; files
      ; sub_dirs
      ; dune_file
      ; project
      }
    ;;

    let source_path = function
      | Source dir -> Source_path.workspace (Workspace_dir.path dir)
      | Mounted { logical_path; _ } -> Source_path.build logical_path
    ;;

    let path = function
      | Source dir -> Path.source (Workspace_dir.path dir)
      | Mounted { logical_path; _ } -> Path.build logical_path
    ;;

    let backing_path = function
      | Source dir -> Path.source (Workspace_dir.path dir)
      | Mounted { backing_path; _ } -> Path.build backing_path
    ;;

    let file t filename =
      match t with
      | Source dir ->
        Path.Source.relative_fname (Workspace_dir.path dir) filename |> File.workspace
      | Mounted { logical_path; layers; _ } ->
        File.mounted ~logical:(Path.Build.relative_fname logical_path filename) ~layers
    ;;

    let relative_dir = function
      | Source dir -> Workspace_dir.path dir |> Path.Source.to_local
      | Mounted { relative_dir; _ } -> relative_dir
    ;;

    let filenames = function
      | Source dir -> Workspace_dir.filenames dir
      | Mounted { files; _ } -> files
    ;;

    let sub_dirs = function
      | Source dir ->
        Workspace_dir.sub_dirs dir
        |> Filename.Array.Map.mapi ~f:(fun _ dir -> Source_sub_dir dir)
      | Mounted { sub_dirs; _ } ->
        Filename.Array.Map.mapi sub_dirs ~f:(fun _ dir -> Mounted_sub_dir dir)
    ;;

    let sub_dir_names t = sub_dirs t |> Filename.Array.Map.keys

    let sub_dir_as_t = function
      | Source_sub_dir dir -> Workspace_dir.sub_dir_as_t dir >>| source
      | Mounted_sub_dir { dir; _ } -> dir >>| fun dir -> Mounted dir
    ;;

    let rec find_dir t path =
      match Path.Local.explode path with
      | [] -> Memo.return (Some t)
      | name :: rest ->
        (match Filename.Array.Map.find (sub_dirs t) name with
         | None -> Memo.return None
         | Some sub_dir ->
           let* sub_dir = sub_dir_as_t sub_dir in
           find_dir sub_dir (Path.Local.of_comps rest))
    ;;

    let status = function
      | Source dir -> Workspace_dir.status dir
      | Mounted { status; _ } -> status
    ;;

    let dune_file = function
      | Source dir -> Workspace_dir.dune_file dir
      | Mounted { dune_file; _ } -> dune_file
    ;;

    let project = function
      | Source dir -> Workspace_dir.project dir
      | Mounted { project; _ } -> project
    ;;

    let to_dyn t =
      Dyn.record
        [ "source_path", Source_path.to_dyn (source_path t)
        ; "backing_path", Path.to_dyn (backing_path t)
        ; "status", Source_dir_status.to_dyn (status t)
        ; ( "files"
          , Dyn.Set (Filename.Array.Set.to_list_map (filenames t) ~f:Filename.to_dyn) )
        ]
    ;;

    module Make_map_reduce (M : Memo.S) (Outcome : Monoid) = struct
      open M.O

      let rec map_reduce t ~traverse ~trace_event_name:_ ~f =
        if not (Source_dir_status.Map.find traverse (status t))
        then M.return Outcome.empty
        else
          let+ here = f t
          and+ children =
            Filename.Array.Map.values (sub_dirs t)
            |> M.List.map ~f:(fun child ->
              let* child = M.of_memo (sub_dir_as_t child) in
              map_reduce child ~traverse ~trace_event_name:"" ~f)
          in
          List.fold_left children ~init:here ~f:Outcome.combine
      ;;
    end
  end

  module Mounted = struct
    module Layer = Source_tree_file.Mounted_layer

    type t =
      { logical_root : Path.Build.t
      ; root : Dir.t
      ; projects : (Dune_project.t * Dir.t) list
      }

    let source_root t = t.logical_root
    let root t = t.root
    let projects t = t.projects

    let read_file ~logical ~layers =
      Source_tree_file.File.mounted ~logical ~layers
      |> Source_tree_file.File.read_with_perm
    ;;

    let directory_contents ~logical_path layers =
      let* entries, physical =
        Memo.List.fold_left
          layers
          ~init:(Filename.Map.empty, false)
          ~f:(fun (entries, physical) layer ->
            match layer with
            | Layer.Directory { logical_root; backing_root } ->
              (match
                 Path.drop_prefix
                   (Path.build logical_path)
                   ~prefix:(Path.build logical_root)
               with
               | None -> Memo.return (entries, physical)
               | Some local ->
                 let backing_path = Path.Build.append_local backing_root local in
                 let+ contents =
                   Build_system.directory_target_contents_opt ~dir:backing_path
                 in
                 (match contents with
                  | None -> entries, physical
                  | Some (files, sub_dirs) ->
                    let entries =
                      Filename.Array.Set.fold files ~init:entries ~f:(fun file entries ->
                        Filename.Map.set entries file `File)
                    in
                    let entries =
                      Filename.Array.Set.fold
                        sub_dirs
                        ~init:entries
                        ~f:(fun sub_dir entries ->
                          Filename.Map.set entries sub_dir `Directory)
                    in
                    entries, true))
            | Layer.File { logical; _ } | Layer.Contents { logical; _ } ->
              (match
                 Path.drop_prefix (Path.build logical) ~prefix:(Path.build logical_path)
               with
               | None -> Memo.return (entries, physical)
               | Some local ->
                 (match Path.Local.split_first_component local with
                  | None ->
                    Code_error.raise
                      "Mounted file layer replaces a directory"
                      [ "path", Path.Build.to_dyn logical ]
                  | Some (basename, rest) ->
                    let kind = if Path.Local.is_root rest then `File else `Directory in
                    Memo.return (Filename.Map.set entries basename kind, true)))
            | Layer.Delete { logical } ->
              (match
                 Path.drop_prefix (Path.build logical) ~prefix:(Path.build logical_path)
               with
               | None -> Memo.return (entries, physical)
               | Some local ->
                 (match Path.Local.split_first_component local with
                  | None ->
                    Code_error.raise
                      "Mounted file deletion targets a directory"
                      [ "path", Path.Build.to_dyn logical ]
                  | Some (basename, rest) ->
                    let entries =
                      if Path.Local.is_root rest
                      then Filename.Map.remove entries basename
                      else entries
                    in
                    Memo.return (entries, physical))))
      in
      let files, sub_dirs =
        Filename.Map.foldi
          entries
          ~init:([], [])
          ~f:(fun filename kind (files, sub_dirs) ->
            match kind with
            | `File -> filename :: files, sub_dirs
            | `Directory -> files, filename :: sub_dirs)
      in
      Memo.return
        (Filename.Array.Set.of_list files, Filename.Array.Set.of_list sub_dirs, physical)
    ;;

    let load_impl (logical_root, layers) =
      let backing_root =
        List.find_map layers ~f:(function
          | Layer.Directory { logical_root = layer_root; backing_root }
            when Path.Build.equal logical_root layer_root -> Some backing_root
          | Directory _ | File _ | Contents _ | Delete _ -> None)
        |> Option.value_exn
      in
      let rec traverse
                ~logical_path
                ~relative_dir
                ~status
                ~parent_dune_file
                ~parent_project
        =
        let* files, physical_subdirs, physical =
          directory_contents ~logical_path layers
        in
        let backing_path = Path.Build.append_local backing_root relative_dir in
        let source_path = Source_path.build logical_path in
        let* loaded_project =
          match status, physical with
          | Source_dir_status.Data_only, _ | _, false -> Memo.return None
          | (Source_dir_status.Normal | Source_dir_status.Vendored), true ->
            Dune_project.gen_load_source
              ~read:(fun path ->
                match Source_path.descendant path ~of_:source_path with
                | Some local ->
                  Source_tree_file.File.mounted
                    ~logical:(Path.Build.append_local logical_path local)
                    ~layers
                  |> Source_tree_file.File.read
                  >>| Option.value_exn
                | None ->
                  Code_error.raise
                    "Mounted project requested a source outside its logical root"
                    [ "source", Source_path.to_dyn path
                    ; "root", Source_path.to_dyn source_path
                    ])
              ~dir:source_path
              ~files
              ~infer_from_opam_files:false
              ~load_opam_file_with_contents:
                Dune_pkg.Opam_file.load_opam_file_with_contents
        in
        let project, is_project_root =
          match loaded_project, parent_project with
          | Some project, _ -> project, true
          | None, Some project -> project, false
          | None, None ->
            ( Dune_project.anonymous
                ~dir:source_path
                Package_info.empty
                Package.Name.Map.empty
            , true )
        in
        let* dune_file =
          Dune_file.load
            ~dir:
              (Source_tree_file.Dir.mounted
                 ~logical:logical_path
                 ~backing:backing_path
                 ~layers)
            status
            project
            ~files
            ~parent:parent_dune_file
        in
        let visible_files =
          let predicate =
            match dune_file with
            | None -> Dune_file.Files.default
            | Some dune_file -> Dune_file.files dune_file
          in
          Dune_file.Files.eval predicate ~files
        in
        let sub_dirs_spec =
          match dune_file with
          | None -> Source_dir_status.Spec.default
          | Some dune_file -> Dune_file.sub_dir_status dune_file
        in
        let all_subdirs =
          match dune_file with
          | None -> physical_subdirs
          | Some dune_file ->
            Filename.Array.Set.union physical_subdirs (Dune_file.sub_dirnames dune_file)
        in
        let status_map = Source_dir_status.Spec.eval sub_dirs_spec ~dirs:all_subdirs in
        let* children =
          Filename.Array.Set.to_list all_subdirs
          |> Memo.List.filter_map ~f:(fun basename ->
            let open Source_dir_status.Or_ignored in
            match Source_dir_status.Per_dir.status status_map ~dir:basename with
            | Ignored -> Memo.return None
            | Status child_status ->
              let child_status =
                if Dune_project.cram project && Cram_test.is_cram_suffix basename
                then Source_dir_status.Data_only
                else (
                  match status, child_status with
                  | Source_dir_status.Data_only, _ -> Source_dir_status.Data_only
                  | Source_dir_status.Vendored, Source_dir_status.Normal ->
                    Source_dir_status.Vendored
                  | _, child_status -> child_status)
              in
              let+ child, child_projects =
                traverse
                  ~logical_path:(Path.Build.relative_fname logical_path basename)
                  ~relative_dir:(Path.Local.relative_fname relative_dir basename)
                  ~status:child_status
                  ~parent_dune_file:dune_file
                  ~parent_project:(Some project)
              in
              Some (basename, child, child_projects))
        in
        let sub_dirs =
          List.map children ~f:(fun (name, child, _) ->
            name, { Dir.dir = Memo.return child })
          |> Filename.Array.Map.of_list_exn
        in
        let projects = List.concat_map children ~f:(fun (_, _, children) -> children) in
        let dir =
          Dir.make_loaded
            ~logical_path
            ~backing_path
            ~layers
            ~relative_dir
            ~status
            ~files:visible_files
            ~sub_dirs
            ~dune_file
            ~project
        in
        let projects =
          if is_project_root then (project, Dir.Mounted dir) :: projects else projects
        in
        Memo.return (dir, projects)
      in
      let+ root, projects =
        traverse
          ~logical_path:logical_root
          ~relative_dir:Path.Local.root
          ~status:Source_dir_status.Vendored
          ~parent_dune_file:None
          ~parent_project:None
      in
      { logical_root; root = Dir.Mounted root; projects }
    ;;

    let load =
      let module Input = struct
        type t = Path.Build.t * Layer.t list

        let equal = Tuple.T2.equal Path.Build.equal (List.equal Layer.equal)
        let hash = Tuple.T2.hash Path.Build.hash (List.hash Layer.hash)
        let to_dyn = Tuple.T2.to_dyn Path.Build.to_dyn (Dyn.list Layer.to_dyn)
      end
      in
      let memo = Memo.create "mounted-source-tree" ~input:(module Input) load_impl in
      fun ~logical_root ~layers -> Memo.exec memo (logical_root, layers)
    ;;
  end
end

module Make_map_reduce_with_progress (M : Memo.S) (Outcome : Monoid) = struct
  open M.O
  include Dir.Make_map_reduce (M) (Outcome)

  let map_reduce ~traverse ~trace_event_name ~f =
    let* root = M.of_memo (root ()) in
    let nb_path_visited = ref 0 in
    let overlay =
      Console.Status_line.add_overlay
        (Live (fun () -> Pp.textf "Scanned %i directories" !nb_path_visited))
    in
    let+ res =
      map_reduce root ~traverse ~trace_event_name ~f:(fun dir ->
        incr nb_path_visited;
        if !nb_path_visited mod 100 = 0 then Console.Status_line.refresh ();
        f dir)
    in
    Console.Status_line.remove_overlay overlay;
    res
  ;;
end

let is_vendored dir =
  find_dir dir
  >>| function
  | None -> false
  | Some d -> Dir.status d = Vendored
;;

let ancestor_vcs =
  Memo.lazy_ ~name:"ancestor_vcs" (fun () ->
    if Execution_env.inside_dune
    then Memo.return None
    else (
      let rec loop dir =
        if Fpath.is_root dir
        then None
        else (
          let dir = Filename.dirname dir in
          match Readdir.read_directory dir with
          | Ok files ->
            let files = Filename.Array.Set.of_list files in
            (match Vcs.Kind.of_dir_contents ~files ~dirs:Filename.Array.Map.empty with
             | Some kind -> Some { Vcs.kind; root = Path.of_string dir }
             | None -> loop dir)
          | Error unix_error ->
            User_warning.emit
              [ Pp.textf
                  "Unable to read directory %s. Will not look for VCS root in parent \
                   directories."
                  dir
              ; Unix_error.Detailed.pp_reason unix_error
              ];
            None)
      in
      Memo.return (loop (Path.to_absolute_filename Path.root))))
;;

let nearest_vcs dir =
  let* dir = nearest_dir dir in
  match dir.vcs with
  | This vcs -> Memo.return (Some vcs)
  | Ancestor_vcs -> Memo.Lazy.force ancestor_vcs
;;
