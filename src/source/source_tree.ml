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
        Filename.Set.union
          (Dir_contents.files readdir)
          (Filename.Set.of_list_map (Dir_contents.dirs readdir) ~f:fst)
        |> Vcs.Kind.of_dir_contents
      with
      | None -> vcs
      | Some kind -> This { Vcs.kind; root = Path.(append_source root) path }
    ;;
  end

  type t =
    { path : Path.Source.t
    ; status : Source_dir_status.t
    ; files : Filename.Set.t
    ; sub_dirs : sub_dir Filename.Map.t
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
      ; "files", Filename.Set.to_dyn files
      ; "sub_dirs", Filename.Map.to_dyn dyn_of_sub_dir sub_dirs
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

  let sub_dir_names t =
    Filename.Map.foldi (sub_dirs t) ~init:Filename.Set.empty ~f:(fun s _ acc ->
      Filename.Set.add acc s)
  ;;

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
    ; Unix_error.Detailed.pp ~prefix:"Reason: " unix_error
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
    Source_dir_status.Spec.eval sub_dirs ~dirs:(List.map ~f:(fun (a, _) -> a) dirs)
  in
  List.fold_left dirs ~init:Filename.Map.empty ~f:(fun subdirs (fn, file) ->
    match eval_status ~status_map ~parent_status fn with
    | None -> subdirs
    | Some dir_status ->
      let path = Path.Source.relative dir fn in
      let dirs_visited = Dirs_visited.add dirs_visited path file in
      { Dir0.sub_dir_status = dir_status
      ; sub_dir_as_t =
          Memo.lazy_cell (fun () ->
            find_dir_raw
              ~default_vcs
              ~path
              ~basename:fn
              ~virtual_:false
              ~dirs_visited
              ~dune_file
              ~status:dir_status
              ~project)
          |> Memo.Cell.read
      }
      |> Filename.Map.add_exn subdirs fn)

and virtual_ ~project ~sub_dirs ~parent_status ~dune_file ~init ~path =
  match dune_file with
  | None -> init
  | Some df ->
    (* There's no files to read for virtual directories, but we still record
       their entries *)
    let dirs = Dune_file.sub_dirnames df in
    let status_map = Source_dir_status.Spec.eval sub_dirs ~dirs in
    List.fold_left dirs ~init ~f:(fun acc fn ->
      match eval_status ~status_map ~parent_status fn with
      | None -> acc
      | Some status ->
        Filename.Map.update acc fn ~f:(function
          (* Physical directories have already been added so they are
             skipped here.

             CR-rgrinberg: we should still update the status for these
             directories if it hasn't been set *)
          | Some _ as r -> r
          | None ->
            Some
              { Dir0.sub_dir_status = status
              ; sub_dir_as_t =
                  Memo.lazy_cell (fun () ->
                    find_dir_raw
                      ~default_vcs:Dir0.Vcs.Ancestor_vcs
                      ~path:(Path.Source.relative path fn)
                      ~basename:fn
                      ~virtual_:true
                      ~dune_file
                      ~status
                      ~dirs_visited:Dirs_visited.empty
                      ~project)
                  |> Memo.Cell.read
              }))

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
  let+ dune_file = Dune_file.load ~dir:path dir_status project ~files ~parent:dune_file in
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
  Memo.lazy_cell
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
     | None -> Dune_project.anonymous ~dir:path Package_info.empty Package.Name.Map.empty)
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
      (match Filename.Map.find dir.sub_dirs x with
       | None -> on_last_found dir
       | Some dir -> dir.sub_dir_as_t >>= loop on_success on_last_found xs)
  in
  fun ~on_success ~on_last_found p ->
    Memo.Cell.read root >>= loop on_success on_last_found (Path.Source.explode p)
;;

let find_dir =
  gen_find_dir
    ~on_success:(fun dir -> Memo.return (Some dir))
    ~on_last_found:(fun _ -> Memo.return None)
;;

let nearest_dir = gen_find_dir ~on_success:Memo.return ~on_last_found:Memo.return
let root () = Memo.Cell.read root

let files_of path =
  find_dir path
  >>| function
  | None -> Path.Source.Set.empty
  | Some dir ->
    Dir0.filenames dir
    |> Filename.Set.to_list
    |> Path.Source.Set.of_list_map ~f:(Path.Source.relative path)
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
            M.List.map (Filename.Map.values t.sub_dirs) ~f:(fun s ->
              let* t = M.of_memo (sub_dir_as_t s) in
              map_reduce t ~traverse ~trace_event_name ~f)
          in
          List.fold_left in_sub_dirs ~init:here ~f:Outcome.combine
      in
      let impl =
        lazy
          (match Dune_trace.global () with
           | None -> map_reduce
           | Some stats ->
             fun t ~traverse ~trace_event_name ~f ->
               let start = Unix.gettimeofday () in
               let+ res = map_reduce t ~traverse ~trace_event_name ~f in
               let stop = Unix.gettimeofday () in
               let event =
                 Dune_trace.Event.scan_source
                   ~name:trace_event_name
                   ~start
                   ~stop
                   ~dir:t.path
               in
               Dune_trace.emit stats event;
               res)
      in
      fun t ~traverse ~trace_event_name ~f ->
        (Lazy.force impl) t ~traverse ~trace_event_name ~f
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
          match
            Sys.readdir dir
            |> Array.to_list
            |> Filename.Set.of_list
            |> Vcs.Kind.of_dir_contents
          with
          | Some kind -> Some { Vcs.kind; root = Path.of_string dir }
          | None -> loop dir
          | exception Sys_error msg ->
            User_warning.emit
              [ Pp.textf
                  "Unable to read directory %s. Will not look for VCS root in parent \
                   directories."
                  dir
              ; Pp.textf "Reason: %s" msg
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
