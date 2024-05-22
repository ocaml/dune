open Import
open Memo.O

module Dirs_visited : sig
  (** Unique set of all directories visited *)
  type t

  val singleton : Path.Source.t -> Readdir.File.t -> t

  module Per_fn : sig
    (** Stores the directories visited per node (basename) *)

    type dirs_visited := t
    type t

    val init : t
    val find : t -> Path.Source.t -> dirs_visited
    val add : t -> dirs_visited -> path:Path.Source.t -> Filename.t * Readdir.File.t -> t
  end
end = struct
  type t = Path.Source.t Readdir.File.Map.t

  let singleton path file = Readdir.File.Map.singleton file path

  module Per_fn = struct
    type nonrec t = t Filename.Map.t

    let init = Filename.Map.empty

    let find t path =
      Path.Source.basename path
      |> Filename.Map.find t
      |> Option.value ~default:Readdir.File.Map.empty
    ;;

    let add (acc : t) dirs_visited ~path (fn, file) =
      if Sys.win32
      then acc
      else (
        let new_dirs_visited =
          Readdir.File.Map.update dirs_visited file ~f:(function
            | None -> Some path
            | Some first_path ->
              User_error.raise
                [ Pp.textf
                    "Path %s has already been scanned. Cannot scan it again through \
                     symlink %s"
                    (Path.Source.to_string_maybe_quoted first_path)
                    (Path.Source.to_string_maybe_quoted path)
                ])
        in
        Filename.Map.add_exn acc fn new_dirs_visited)
    ;;
  end
end

module Output = struct
  type 'a t =
    { dir : 'a
    ; visited : Dirs_visited.Per_fn.t
    }
end

module Dir0 = struct
  module Vcs = struct
    type nonrec t =
      | This of Vcs.t
      | Ancestor_vcs

    let get_vcs ~default:vcs ~readdir ~path =
      match
        Filename.Set.union
          (Readdir.files readdir)
          (Filename.Set.of_list_map (Readdir.dirs readdir) ~f:fst)
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
    ; dune_file : Dune_file0.t option
    ; project : Dune_project.t
    ; vcs : Vcs.t
    }

  and sub_dir =
    { sub_dir_status : Source_dir_status.t
    ; virtual_ : bool
    ; sub_dir_as_t : (Path.Source.t, t Output.t option) Memo.Cell.t
    }

  let rec to_dyn { path; status; files; dune_file; sub_dirs; vcs = _; project = _ } =
    let open Dyn in
    Record
      [ "path", Path.Source.to_dyn path
      ; "status", Source_dir_status.to_dyn status
      ; "files", Filename.Set.to_dyn files
      ; "sub_dirs", Filename.Map.to_dyn dyn_of_sub_dir sub_dirs
      ; ("dune_file", Dyn.(option opaque dune_file))
      ]

  and dyn_of_sub_dir { sub_dir_status; sub_dir_as_t; virtual_ } =
    let open Dyn in
    let path = Memo.Cell.input sub_dir_as_t in
    record
      [ "status", Source_dir_status.to_dyn sub_dir_status
      ; "sub_dir_as_t", Path.Source.to_dyn path
      ; "virtual_", bool virtual_
      ]
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

  let sub_dir_as_t (s : sub_dir) =
    let+ t = Memo.Cell.read s.sub_dir_as_t in
    (Option.value_exn t).dir
  ;;
end

module rec Memoized : sig
  val root : unit -> Dir0.t Memo.t

  (* Not part of the interface. Only necessary to call recursively *)
  val find_dir_raw : Path.Source.t -> (Path.Source.t, Dir0.t Output.t option) Memo.Cell.t
  val find_dir : Path.Source.t -> Dir0.t option Memo.t
end = struct
  open Memoized

  module Get_subdir = struct
    let status ~status_map ~(parent_status : Source_dir_status.t) dir
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

    let make_subdir ~dir_status ~virtual_ path =
      let sub_dir_as_t = find_dir_raw path in
      { Dir0.sub_dir_status = dir_status; sub_dir_as_t; virtual_ }
    ;;

    let physical ~dir ~dirs_visited ~dirs ~sub_dirs ~parent_status =
      let status_map =
        Source_dir_status.Spec.eval sub_dirs ~dirs:(List.map ~f:(fun (a, _) -> a) dirs)
      in
      List.fold_left
        dirs
        ~init:(Dirs_visited.Per_fn.init, Filename.Map.empty)
        ~f:(fun (dirs_visited_acc, subdirs) ((fn, _) as info) ->
          match status ~status_map ~parent_status fn with
          | None -> dirs_visited_acc, subdirs
          | Some dir_status ->
            let dirs_visited_acc =
              let path = Path.Source.relative dir fn in
              Dirs_visited.Per_fn.add dirs_visited_acc dirs_visited ~path info
            in
            let subdirs =
              Path.Source.relative dir fn
              |> make_subdir ~dir_status ~virtual_:false
              |> Filename.Map.add_exn subdirs fn
            in
            dirs_visited_acc, subdirs)
    ;;

    let virtual_ ~sub_dirs ~parent_status ~dune_file ~init ~path =
      match dune_file with
      | None -> init
      | Some df ->
        (* There's no files to read for virtual directories, but we still record
           their entries *)
        let dirs = Dune_file0.sub_dirnames df in
        let status_map = Source_dir_status.Spec.eval sub_dirs ~dirs in
        List.fold_left dirs ~init ~f:(fun acc fn ->
          match status ~status_map ~parent_status fn with
          | None -> acc
          | Some dir_status ->
            Filename.Map.update acc fn ~f:(function
              (* Physical directories have already been added so they are
                 skipped here.

                 CR-rgrinberg: we should still update the status for these
                 directories if it hasn't been set *)
              | Some _ as r -> r
              | None ->
                let path = Path.Source.relative path fn in
                (* CR-rgrinberg: we could introduce a simplified
                   call paths for virtual directories since we know the
                   children of a virtual directory are also virtual. *)
                Some (make_subdir ~dir_status ~virtual_:true path)))
    ;;

    let all ~dirs_visited ~dirs ~sub_dirs ~parent_status ~dune_file ~path =
      let visited, init =
        physical ~dir:path ~dirs_visited ~dirs ~sub_dirs ~parent_status
      in
      let init = virtual_ ~sub_dirs ~parent_status ~dune_file ~init ~path in
      visited, init
    ;;
  end

  let contents
    readdir
    ~vcs
    ~path
    ~parent_dune_file
    ~dirs_visited
    ~project
    ~(dir_status : Source_dir_status.t)
    =
    let files = Readdir.files readdir in
    let+ dune_file =
      Dune_file0.load ~dir:path dir_status project ~files ~parent:parent_dune_file
    in
    let dirs_visited, sub_dirs =
      let sub_dirs =
        match dune_file with
        | None -> Source_dir_status.Spec.default
        | Some dune_file -> Dune_file0.sub_dir_status dune_file
      in
      Get_subdir.all
        ~dirs_visited
        ~dirs:(Readdir.dirs readdir)
        ~sub_dirs
        ~parent_status:dir_status
        ~dune_file
        ~path
    in
    ( { Dir0.project; vcs; status = dir_status; path; files; sub_dirs; dune_file }
    , dirs_visited )
  ;;

  let error_unable_to_load ~path unix_error =
    User_error.raise
      [ Pp.textf "Unable to load source %s." (Path.Source.to_string_maybe_quoted path)
      ; Unix_error.Detailed.pp ~prefix:"Reason: " unix_error
      ]
  ;;

  let root () =
    let path = Path.Source.root in
    let dir_status : Source_dir_status.t = Normal in
    let+ dir, visited =
      let* readdir =
        Readdir.of_source_path path
        >>| function
        | Ok dir -> dir
        | Error unix_error -> error_unable_to_load ~path unix_error
      in
      let* project =
        Dune_project.load
          ~dir:path
          ~files:(Readdir.files readdir)
          ~infer_from_opam_files:true
        >>| (function
               | Some p -> p
               | None ->
                 Dune_project.anonymous
                   ~dir:path
                   Package_info.empty
                   Package.Name.Map.empty)
        >>| Only_packages.filter_packages_in_project ~vendored:(dir_status = Vendored)
      in
      let vcs = Dir0.Vcs.get_vcs ~default:Dir0.Vcs.Ancestor_vcs ~readdir ~path in
      let* dirs_visited =
        Readdir.File.of_source_path (In_source_dir path)
        >>| function
        | Ok file -> Dirs_visited.singleton path file
        | Error unix_error -> error_unable_to_load ~path unix_error
      in
      contents
        readdir
        ~vcs
        ~path
        ~parent_dune_file:None
        ~dirs_visited
        ~project
        ~dir_status
    in
    { Output.dir; visited }
  ;;

  let find_dir_raw_impl path : Dir0.t Output.t option Memo.t =
    match Path.Source.parent path with
    | None ->
      let+ root = root () in
      Some root
    | Some parent_dir ->
      let* parent = Memo.Cell.read (find_dir_raw parent_dir) in
      (match
         let open Option.O in
         let* { Output.dir = parent_dir; visited = dirs_visited } = parent in
         let+ dir_status, virtual_ =
           let basename = Path.Source.basename path in
           let+ sub_dir = Filename.Map.find parent_dir.sub_dirs basename in
           let status =
             let status = sub_dir.sub_dir_status in
             if Dune_project.cram parent_dir.project && Cram_test.is_cram_suffix basename
             then Source_dir_status.Data_only
             else status
           in
           status, sub_dir.virtual_
         in
         parent_dir, dirs_visited, dir_status, virtual_
       with
       | None -> Memo.return None
       | Some (parent_dir, dirs_visited, dir_status, virtual_) ->
         let* readdir =
           if virtual_
           then Memo.return Readdir.empty
           else
             Readdir.of_source_path path
             >>| function
             | Ok dir -> dir
             | Error _ -> Readdir.empty
         in
         let* project =
           if dir_status = Data_only
           then Memo.return parent_dir.project
           else
             Dune_project.load
               ~dir:path
               ~files:(Readdir.files readdir)
               ~infer_from_opam_files:false
             >>| Option.map
                   ~f:
                     (Only_packages.filter_packages_in_project
                        ~vendored:(dir_status = Vendored))
             >>| Option.value ~default:parent_dir.project
         in
         let vcs = Dir0.Vcs.get_vcs ~default:parent_dir.vcs ~readdir ~path in
         let+ dir, visited =
           let dirs_visited = Dirs_visited.Per_fn.find dirs_visited path in
           contents
             readdir
             ~vcs
             ~path
             ~parent_dune_file:parent_dir.dune_file
             ~dirs_visited
             ~project
             ~dir_status
         in
         Some { Output.dir; visited })
  ;;

  let find_dir_raw =
    let memo =
      (* amokhov: After running some experiments, I convinced myself that it's
         not worth adding a [cutoff] here because we don't recompute this
         function very often (the [find_dir] calls are probably guarded by other
         cutoffs). Note also that adding a [cutoff] here is non-trivial because
         [Dir0.t] stores memoization cells in [sub_dir_as_t]. *)
      Memo.create "find-dir-raw" ~input:(module Path.Source) find_dir_raw_impl
    in
    Memo.cell memo
  ;;

  let find_dir p =
    Memo.Cell.read (find_dir_raw p)
    >>| function
    | Some { Output.dir; visited = _ } -> Some dir
    | None -> None
  ;;

  let root () = find_dir Path.Source.root >>| Option.value_exn
end

let root () = Memoized.root ()
let find_dir path = Memoized.find_dir path

let rec nearest_dir t = function
  | [] -> Memo.return t
  | comp :: components ->
    (match Filename.Map.find (Dir0.sub_dirs t) comp with
     | None -> Memo.return t
     | Some sub_dir ->
       let* sub_dir = Dir0.sub_dir_as_t sub_dir in
       nearest_dir sub_dir components)
;;

let nearest_dir path =
  let components = Path.Source.explode path in
  let* root = root () in
  nearest_dir root components
;;

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
      let rec map_reduce t ~traverse ~f =
        let must_traverse = Source_dir_status.Map.find traverse t.status in
        match must_traverse with
        | false -> M.return Outcome.empty
        | true ->
          let+ here = f t
          and+ in_sub_dirs =
            M.List.map (Filename.Map.values t.sub_dirs) ~f:(fun s ->
              let* t = M.of_memo (sub_dir_as_t s) in
              map_reduce t ~traverse ~f)
          in
          List.fold_left in_sub_dirs ~init:here ~f:Outcome.combine
      in
      let impl =
        lazy
          (match Dune_stats.global () with
           | None -> map_reduce
           | Some stats ->
             fun t ~traverse ~f ->
               let start = Unix.gettimeofday () in
               let+ res = map_reduce t ~traverse ~f in
               let event =
                 let stop = Unix.gettimeofday () in
                 let module Event = Chrome_trace.Event in
                 let module Timestamp = Event.Timestamp in
                 let dur = Timestamp.of_float_seconds (stop -. start) in
                 let common =
                   Event.common_fields
                     ~name:"Source tree scan"
                     ~ts:(Timestamp.of_float_seconds start)
                     ()
                 in
                 let args = [ "dir", `String (Path.Source.to_string t.path) ] in
                 Event.complete common ~args ~dur
               in
               Dune_stats.emit stats event;
               res)
      in
      fun t ~traverse ~f -> (Lazy.force impl) t ~traverse ~f
    ;;
  end
end

module Make_map_reduce_with_progress (M : Memo.S) (Outcome : Monoid) = struct
  open M.O
  include Dir.Make_map_reduce (M) (Outcome)

  let map_reduce ~traverse ~f =
    let* root = M.of_memo (root ()) in
    let nb_path_visited = ref 0 in
    let overlay =
      Console.Status_line.add_overlay
        (Live (fun () -> Pp.textf "Scanned %i directories" !nb_path_visited))
    in
    let+ res =
      map_reduce root ~traverse ~f:(fun dir ->
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
          | None -> loop dir)
      in
      Memo.return (loop (Path.to_absolute_filename Path.root))))
;;

let nearest_vcs dir =
  let* dir = nearest_dir dir in
  match dir.vcs with
  | This vcs -> Memo.return (Some vcs)
  | Ancestor_vcs -> Memo.Lazy.force ancestor_vcs
;;
