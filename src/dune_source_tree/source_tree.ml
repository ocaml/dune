open Stdune
open Memo.O

module Dirs_visited : sig
  (** Unique set of all directories visited *)
  type t

  val singleton : Path.Source.t -> File.t -> t

  module Per_fn : sig
    (** Stores the directories visited per node (basename) *)

    type dirs_visited := t

    type t

    val init : t

    val find : t -> Path.Source.t -> dirs_visited

    val add : t -> dirs_visited -> Filename.t * Path.Source.t * File.t -> t
  end
end = struct
  type t = Path.Source.t File.Map.t

  let singleton path file = File.Map.singleton file path

  module Per_fn = struct
    type nonrec t = t Filename.Map.t

    let init = Filename.Map.empty

    let find t path =
      Filename.Map.find t (Path.Source.basename path)
      |> Option.value ~default:File.Map.empty

    let add (acc : t) dirs_visited (fn, path, file) =
      if Sys.win32 then acc
      else
        let new_dirs_visited =
          File.Map.update dirs_visited file ~f:(function
            | None -> Some path
            | Some first_path ->
              User_error.raise
                [ Pp.textf
                    "Path %s has already been scanned. Cannot scan it again \
                     through symlink %s"
                    (Path.Source.to_string_maybe_quoted first_path)
                    (Path.Source.to_string_maybe_quoted path)
                ])
        in
        Filename.Map.add_exn acc fn new_dirs_visited
  end
end

module Output = struct
  type 'a t =
    { dir : 'a
    ; visited : Dirs_visited.Per_fn.t
    }
end

module Dir = struct
  type 'a t =
    { path : Path.Source.t
    ; files : Filename.Set.t
    ; sub_dirs : 'a sub_dir Filename.Map.t
    ; value : 'a
    }

  and 'a sub_dir =
    { sub_dir_as_t : (Path.Source.t, 'a t Output.t option) Memo.Cell.t }

  let value t = t.value

  let to_dyn { path; files; sub_dirs; value = _ } =
    Dyn.record
      [ ("path", Path.Source.to_dyn path)
      ; ("files", Filename.Set.to_dyn files)
      ; ("sub_dirs", Filename.Map.to_dyn Dyn.opaque sub_dirs)
      ]

  let path t = t.path

  let files t = t.files

  let sub_dirs t = t.sub_dirs

  let file_paths t =
    Path.Source.Set.of_listing ~dir:t.path
      ~filenames:(Filename.Set.to_list (files t))

  let sub_dir_names t =
    Filename.Map.foldi (sub_dirs t) ~init:Filename.Set.empty ~f:(fun s _ acc ->
        Filename.Set.add acc s)

  let sub_dir_as_t (s : _ sub_dir) =
    let+ t = Memo.Cell.read s.sub_dir_as_t in
    (Option.value_exn t).dir

  module Make_map_reduce (M : Memo.S) (Outcome : Monoid) = struct
    open M.O

    let rec map_reduce t ~f =
      let+ here = f t
      and+ in_sub_dirs =
        Filename.Map.values t.sub_dirs
        |> M.List.map ~f:(fun s ->
               let* t = M.of_memo (sub_dir_as_t s) in
               map_reduce t ~f)
      in
      List.fold_left in_sub_dirs ~init:here ~f:Outcome.combine
  end
end

module Make
    (Reduced_stats : Reduced_stats_intf.S)
    (File_memo : Readdir.S with type reduced_stats := Reduced_stats.t) =
struct
  module File_memo = struct
    module File_of_reduced_stats = File.Make (Reduced_stats)
    include File_memo

    let stat dir =
      let open Memo.O in
      let+ res = stat dir in
      Result.map res ~f:File_of_reduced_stats.of_stats
  end

  type 'a t =
    { root : 'a Dir.t Memo.t
    ; find_dir : Path.Source.t -> 'a Dir.t option Memo.t
    }

  let create (type a) (value : a) =
    let module Non_rec = struct
      module rec Memoized : sig
        (* Not part of the interface. Only necessary to call recursively *)
        val find_dir_raw :
          Path.Source.t -> (Path.Source.t, a Dir.t Output.t option) Memo.Cell.t

        val find_dir : Path.Source.t -> a Dir.t option Memo.t
      end = struct
        open Memoized

        let contents { Readdir.path = _; dirs; files } ~dirs_visited =
          let dirs_visited, sub_dirs =
            List.fold_left dirs
              ~init:(Dirs_visited.Per_fn.init, Filename.Map.empty)
              ~f:(fun (dirs_visited_acc, subdirs) ((fn, path, _) as dir) ->
                let dirs_visited_acc =
                  Dirs_visited.Per_fn.add dirs_visited_acc dirs_visited dir
                in
                let sub_dir =
                  let sub_dir_as_t = find_dir_raw path in
                  { Dir.sub_dir_as_t }
                in
                let subdirs = Filename.Map.add_exn subdirs fn sub_dir in
                (dirs_visited_acc, subdirs))
          in
          (files, sub_dirs, dirs_visited)

        let root () =
          let path = Path.Source.root in
          let error_unable_to_load ~path unix_error =
            User_error.raise
              [ Pp.textf "Unable to load source %s."
                  (Path.Source.to_string_maybe_quoted path)
              ; Unix_error.Detailed.pp ~prefix:"Reason: " unix_error
              ]
          in
          let* readdir =
            File_memo.readdir path >>| function
            | Ok dir -> dir
            | Error unix_error -> error_unable_to_load ~path unix_error
          in
          let+ dirs_visited =
            File_memo.stat (In_source_dir path) >>| function
            | Error unix_error -> error_unable_to_load ~path unix_error
            | Ok file -> Dirs_visited.singleton path file
          in
          let files, sub_dirs, visited = contents readdir ~dirs_visited in
          let dir = { Dir.path; files; sub_dirs; value } in
          { Output.dir; visited }

        let find_dir_raw_impl path : a Dir.t Output.t option Memo.t =
          match Path.Source.parent path with
          | None ->
            let+ root = root () in
            Some root
          | Some parent_dir -> (
            let* parent = Memo.Cell.read (find_dir_raw parent_dir) in
            match
              let open Option.O in
              let+ { Output.dir = _parent_dir; visited = dirs_visited } =
                parent
              in
              dirs_visited
            with
            | None -> Memo.return None
            | Some dirs_visited ->
              let dirs_visited = Dirs_visited.Per_fn.find dirs_visited path in
              let+ readdir =
                File_memo.readdir path >>| function
                | Ok dir -> dir
                | Error _ -> Readdir.empty path
              in
              let files, sub_dirs, visited = contents readdir ~dirs_visited in
              let dir = { Dir.path; files; sub_dirs; value } in
              Some { Output.dir; visited })

        let find_dir_raw =
          let memo =
            (* amokhov: After running some experiments, I convinced myself that it's
               not worth adding a [cutoff] here because we don't recompute this
               function very often (the [find_dir] calls are probably guarded by other
               cutoffs). Note also that adding a [cutoff] here is non-trivial because
               [Dir.t] stores memoization cells in [sub_dir_as_t]. *)
            Memo.create "find-dir-raw"
              ~input:(module Path.Source)
              find_dir_raw_impl
          in
          Memo.cell memo

        let find_dir p =
          Memo.Cell.read (find_dir_raw p) >>| function
          | Some { Output.dir; visited = _ } -> Some dir
          | None -> None
      end
    end in
    let open Non_rec.Memoized in
    let root = find_dir Path.Source.root >>| Option.value_exn in
    { root; find_dir }

  let root t = t.root

  let find_dir t path = t.find_dir path

  let rec nearest_dir t = function
    | [] -> Memo.return t
    | comp :: components -> (
      match Filename.Map.find (Dir.sub_dirs t) comp with
      | None -> Memo.return t
      | Some sub_dir ->
        let* sub_dir = Dir.sub_dir_as_t sub_dir in
        nearest_dir sub_dir components)

  let nearest_dir t path =
    let components = Path.Source.explode path in
    let* root = t.root in
    nearest_dir root components

  let files_of t path =
    find_dir t path >>| function
    | None -> Path.Source.Set.empty
    | Some dir ->
      Dir.files dir |> Filename.Set.to_list
      |> Path.Source.Set.of_list_map ~f:(Path.Source.relative path)

  let file_exists t path =
    find_dir t (Path.Source.parent_exn path) >>| function
    | None -> false
    | Some dir -> Filename.Set.mem (Dir.files dir) (Path.Source.basename path)

  let dir_exists t path = find_dir t path >>| Option.is_some
end
