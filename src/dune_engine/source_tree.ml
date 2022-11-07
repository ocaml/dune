open Import
open Memo.O

module File = struct
  module T = struct
    type t =
      { ino : int
      ; dev : int
      }

    let to_dyn { ino; dev } =
      let open Dyn in
      record [ ("ino", Int.to_dyn ino); ("dev", Int.to_dyn dev) ]

    let compare { ino; dev } t =
      let open Ordering.O in
      let= () = Int.compare ino t.ino in
      Int.compare dev t.dev
  end

  include T

  let dummy = { ino = 0; dev = 0 }

  let of_stats (st : Fs_cache.Reduced_stats.t) =
    { ino = st.st_ino; dev = st.st_dev }

  module Map = Map.Make (T)

  let of_source_path p = Fs_memo.path_stat p >>| Result.map ~f:of_stats
end

module Dune_file = struct
  module Plain = struct
    type t =
      { contents : Sub_dirs.Dir_map.per_dir
      ; for_subdirs : Sub_dirs.Dir_map.t
      }
  end

  let fname = "dune"

  let alternative_fname = "dune-file"

  type kind =
    | Plain
    | Ocaml_script

  type t =
    { path : Path.Source.t
    ; kind : kind
    ; (* for [kind = Ocaml_script], this is the part inserted with subdir *)
      plain : Plain.t
    }

  let get_static_sexp t = t.plain.contents.sexps

  let kind t = t.kind

  let path t = t.path

  let sub_dirs (t : t option) =
    match t with
    | None -> Sub_dirs.default
    | Some t -> Sub_dirs.or_default t.plain.contents.subdir_status

  let load_plain sexps ~file ~from_parent ~project =
    let+ active =
      let+ parsed =
        let decoder =
          { Sub_dirs.decode =
              (fun ast d ->
                let d = Dune_project.set_parsing_context project d in
                Dune_lang.Decoder.parse d Univ_map.empty
                  (Dune_lang.Ast.List (Loc.none, ast)))
          }
        in
        Sub_dirs.decode ~file decoder sexps
      in
      match from_parent with
      | None -> parsed
      | Some from_parent -> Sub_dirs.Dir_map.merge parsed from_parent
    in
    let contents = Sub_dirs.Dir_map.root active in
    { Plain.contents; for_subdirs = active }

  let load file ~file_exists ~from_parent ~project =
    let+ kind, plain =
      let load_plain = load_plain ~file ~from_parent ~project in
      match file_exists with
      | false ->
        let+ plain = load_plain [] in
        (Plain, plain)
      | true ->
        let* kind, ast =
          Fs_memo.with_lexbuf_from_file (In_source_dir file) ~f:(fun lb ->
              let kind, ast =
                if Dune_lang.Dune_file_script.is_script lb then
                  (Ocaml_script, [])
                else (Plain, Dune_lang.Parser.parse lb ~mode:Many)
              in
              (kind, ast))
        in
        let+ ast = load_plain ast in
        (kind, ast)
    in
    { path = file; kind; plain }
end

let filter_source_files =
  let is_temp_file fn =
    String.is_prefix fn ~prefix:".#"
    || String.is_suffix fn ~suffix:".swp"
    || String.is_suffix fn ~suffix:"~"
  in
  ref (fun _ -> Memo.return (fun _dir fn -> not (is_temp_file fn)))

module Readdir : sig
  type t = private
    { path : Path.Source.t
    ; files : String.Set.t
    ; dirs : (string * Path.Source.t * File.t) list
    }

  val empty : Path.Source.t -> t

  val filter_files : t -> Dune_project.t -> t Memo.t

  val of_source_path :
    Path.Source.t -> (t, Unix_error.Detailed.t) Result.t Memo.t
end = struct
  type t =
    { path : Path.Source.t
    ; files : String.Set.t
    ; dirs : (string * Path.Source.t * File.t) list
    }

  let equal =
    let dirs_equal (s1, p1, f1) (s2, p2, f2) =
      String.equal s1 s2 && Path.Source.equal p1 p2 && File.compare f1 f2 = Eq
    in
    fun x y ->
      Path.Source.equal x.path y.path
      && String.Set.equal x.files y.files
      && List.equal dirs_equal x.dirs y.dirs

  let empty path = { path; files = String.Set.empty; dirs = [] }

  let _to_dyn { path; files; dirs } =
    let open Dyn in
    record
      [ ("path", Path.Source.to_dyn path)
      ; ("files", String.Set.to_dyn files)
      ; ("dirs", list (triple string Path.Source.to_dyn File.to_dyn) dirs)
      ]

  (* Returns [true] for special files such as character devices of sockets; see
     #3124 for more on issues caused by special devices *)
  let is_special (st_kind : Unix.file_kind) =
    match st_kind with
    | S_CHR | S_BLK | S_FIFO | S_SOCK -> true
    | _ -> false

  let filter_files t project =
    let+ f = !filter_source_files project in
    { t with files = String.Set.filter t.files ~f:(fun fn -> f t.path fn) }

  let of_source_path_impl path =
    Fs_memo.dir_contents (In_source_dir path) >>= function
    | Error unix_error ->
      User_warning.emit
        [ Pp.textf "Unable to read directory %s. Ignoring."
            (Path.Source.to_string_maybe_quoted path)
        ; Pp.text "Remove this message by ignoring by adding:"
        ; Pp.textf "(dirs \\ %s)" (Path.Source.basename path)
        ; Pp.textf "to the dune file: %s"
            (Path.Source.to_string_maybe_quoted
               (Path.Source.relative
                  (Path.Source.parent_exn path)
                  Dune_file.fname))
        ; Unix_error.Detailed.pp ~prefix:"Reason: " unix_error
        ];
      Memo.return (Error unix_error)
    | Ok dir_contents ->
      let dir_contents = Fs_cache.Dir_contents.to_list dir_contents in
      let+ files, dirs =
        Memo.parallel_map dir_contents ~f:(fun (fn, kind) ->
            let path = Path.Source.relative path fn in
            if Path.Source.is_in_build_dir path then Memo.return List.Skip
            else
              let+ is_directory, file =
                match kind with
                | S_DIR -> (
                  File.of_source_path (In_source_dir path) >>| function
                  | Ok file -> (true, file)
                  | Error _ -> (true, File.dummy))
                | S_LNK -> (
                  Fs_memo.path_stat (In_source_dir path) >>| function
                  | Ok ({ st_kind = S_DIR; _ } as st) -> (true, File.of_stats st)
                  | Ok _ | Error _ -> (false, File.dummy))
                | _ -> Memo.return (false, File.dummy)
              in
              if is_directory then List.Right (fn, path, file)
              else if is_special kind then Skip
              else Left fn)
        >>| List.filter_partition_map ~f:Fun.id
      in
      { path; files = String.Set.of_list files; dirs } |> Result.ok

  (* Having a cutoff here speeds up incremental rebuilds quite a bit when a
     directory contents is invalidated but the result stays the same. *)
  let of_source_path_memo =
    Memo.create "readdir-of-source-path"
      ~input:(module Path.Source)
      ~cutoff:(Result.equal equal Unix_error.Detailed.equal)
      of_source_path_impl

  let of_source_path = Memo.exec of_source_path_memo
end

module Dirs_visited : sig
  (** Unique set of all directories visited *)
  type t

  val singleton : Path.Source.t -> File.t -> t

  module Per_fn : sig
    (** Stores the directories visited per node (basename) *)
    type t

    type dirs_visited

    val init : t

    val find : t -> Path.Source.t -> dirs_visited

    val add : t -> dirs_visited -> string * Path.Source.t * File.t -> t
  end
  with type dirs_visited := t
end = struct
  type t = Path.Source.t File.Map.t

  let singleton path file = File.Map.singleton file path

  module Per_fn = struct
    type nonrec t = t String.Map.t

    let init = String.Map.empty

    let find t path =
      String.Map.find t (Path.Source.basename path)
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
        String.Map.add_exn acc fn new_dirs_visited
  end
end

module Output = struct
  type 'a t =
    { dir : 'a
    ; visited : Dirs_visited.Per_fn.t
    }
end

module Dir0 = struct
  type vcs =
    | Ancestor_vcs
    | This of Vcs.t

  type t =
    { path : Path.Source.t
    ; status : Sub_dirs.Status.t
    ; contents : contents
    ; project : Dune_project.t
    ; vcs : vcs
    }

  and contents =
    { files : String.Set.t
    ; sub_dirs : sub_dir String.Map.t
    ; dune_file : Dune_file.t option
    }

  and sub_dir =
    { sub_dir_status : Sub_dirs.Status.t
    ; virtual_ : bool
    ; sub_dir_as_t : (Path.Source.t, t Output.t option) Memo.Cell.t
    }

  type error = Missing_run_t of Cram_test.t

  let rec to_dyn { path; status; contents; project = _; vcs } =
    let open Dyn in
    Record
      [ ("path", Path.Source.to_dyn path)
      ; ("status", Sub_dirs.Status.to_dyn status)
      ; ("contents", dyn_of_contents contents)
      ; ( "vcs"
        , match vcs with
          | Ancestor_vcs -> Dyn.Variant ("Ancestor_vcs", [])
          | This vcs -> Dyn.Variant ("This", [ Vcs.to_dyn vcs ]) )
      ]

  and dyn_of_sub_dir { sub_dir_status; sub_dir_as_t; virtual_ } =
    let open Dyn in
    let path = Memo.Cell.input sub_dir_as_t in
    record
      [ ("status", Sub_dirs.Status.to_dyn sub_dir_status)
      ; ("sub_dir_as_t", Path.Source.to_dyn path)
      ; ("virtual_", bool virtual_)
      ]

  and dyn_of_contents { files; sub_dirs; dune_file } =
    let open Dyn in
    record
      [ ("files", String.Set.to_dyn files)
      ; ("sub_dirs", String.Map.to_dyn dyn_of_sub_dir sub_dirs)
      ; ("dune_file", Dyn.(option opaque dune_file))
      ; ("project", Opaque)
      ]

  module Contents = struct
    let create ~files ~sub_dirs ~dune_file = { files; sub_dirs; dune_file }
  end

  let create ~project ~path ~status ~contents ~vcs =
    { path; status; contents; project; vcs }

  let contents t = t.contents

  let path t = t.path

  let status t = t.status

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
    String.Map.foldi (sub_dirs t) ~init:Path.Source.Set.empty ~f:(fun s _ acc ->
        Path.Source.Set.add acc (Path.Source.relative t.path s))

  let sub_dir_as_t (s : sub_dir) =
    let+ t = Memo.Cell.read s.sub_dir_as_t in
    (Option.value_exn t).dir
end

let ancestor_vcs =
  Memo.lazy_ ~name:"ancestor_vcs" (fun () ->
      if Config.inside_dune then Memo.return None
      else
        let rec loop dir =
          if Fpath.is_root dir then None
          else
            let dir = Filename.dirname dir in
            match
              Sys.readdir dir |> Array.to_list |> String.Set.of_list
              |> Vcs.Kind.of_dir_contents
            with
            | Some kind -> Some { Vcs.kind; root = Path.of_string dir }
            | None -> loop dir
        in
        Memo.return (loop (Path.to_absolute_filename Path.root)))

module rec Memoized : sig
  val root : unit -> Dir0.t Memo.t

  (* Not part of the interface. Only necessary to call recursively *)
  val find_dir_raw :
    Path.Source.t -> (Path.Source.t, Dir0.t Output.t option) Memo.Cell.t

  val find_dir : Path.Source.t -> Dir0.t option Memo.t
end = struct
  open Memoized

  module Get_subdir : sig
    (** Get all the sub directories of [path].*)
    val all :
         dirs_visited:Dirs_visited.t
      -> dirs:(string * Path.Source.t * File.t) list
      -> sub_dirs:Predicate_lang.Glob.t Sub_dirs.Status.Map.t
      -> parent_status:Sub_dirs.Status.t
      -> dune_file:Dune_file.t option (** to interpret [(subdir ..)] stanzas *)
      -> path:Path.Source.t
      -> Dirs_visited.Per_fn.t * Dir0.sub_dir String.Map.t
  end = struct
    let status ~status_map ~(parent_status : Sub_dirs.Status.t) dir :
        Sub_dirs.Status.t option =
      let status = Sub_dirs.status status_map ~dir in
      match status with
      | Ignored -> None
      | Status status ->
        Some
          (match (parent_status, status) with
          | Data_only, _ -> Data_only
          | Vendored, Normal -> Vendored
          | _, _ -> status)

    let make_subdir ~dir_status ~virtual_ path =
      let sub_dir_as_t = find_dir_raw path in
      { Dir0.sub_dir_status = dir_status; sub_dir_as_t; virtual_ }

    let physical ~dirs_visited ~dirs ~sub_dirs ~parent_status =
      let status_map =
        Sub_dirs.eval sub_dirs ~dirs:(List.map ~f:(fun (a, _, _) -> a) dirs)
      in
      List.fold_left dirs ~init:(Dirs_visited.Per_fn.init, String.Map.empty)
        ~f:(fun (dirs_visited_acc, subdirs) ((fn, path, _) as dir) ->
          match status ~status_map ~parent_status fn with
          | None -> (dirs_visited_acc, subdirs)
          | Some dir_status ->
            let dirs_visited_acc =
              Dirs_visited.Per_fn.add dirs_visited_acc dirs_visited dir
            in
            let sub_dir = make_subdir ~dir_status ~virtual_:false path in
            let subdirs = String.Map.add_exn subdirs fn sub_dir in
            (dirs_visited_acc, subdirs))

    let virtual_ ~sub_dirs ~parent_status ~dune_file ~init ~path =
      match dune_file with
      | None -> init
      | Some (df : Dune_file.t) ->
        (* Virtual directories are not in [Readdir.t]. Their presence is only *)
        let dirs = Sub_dirs.Dir_map.sub_dirs df.plain.for_subdirs in
        let status_map = Sub_dirs.eval sub_dirs ~dirs in
        List.fold_left dirs ~init ~f:(fun acc fn ->
            let path = Path.Source.relative path fn in
            match status ~status_map ~parent_status fn with
            | None -> acc
            | Some dir_status ->
              String.Map.update acc fn ~f:(function
                (* Physical directories have already been added so they are
                   skipped here.*)
                | Some _ as r -> r
                | None -> Some (make_subdir ~dir_status ~virtual_:true path)))

    let all ~dirs_visited ~dirs ~sub_dirs ~parent_status ~dune_file ~path =
      let visited, init =
        physical ~dirs_visited ~dirs ~sub_dirs ~parent_status
      in
      let init = virtual_ ~sub_dirs ~parent_status ~dune_file ~init ~path in
      (visited, init)
  end

  let ensure_dune_project_file_exists =
    let memo =
      let module Input = struct
        type t = [ `Is_error of bool ] * Dune_project.t

        let equal (a1, p1) (a2, p2) =
          Poly.equal a1 a2 && Dune_project.equal p1 p2

        let hash = Tuple.T2.hash Poly.hash Dune_project.hash

        let to_dyn (`Is_error b, project) =
          Dyn.(pair bool Dune_project.to_dyn) (b, project)
      end in
      Memo.create "ensure-dune-project-file-exists"
        ~input:(module Input)
        (fun (`Is_error is_error, project) ->
          let open Memo.O in
          let+ exists =
            Path.Outside_build_dir.In_source_dir (Dune_project.file project)
            |> Fs_memo.file_exists
          in
          if not exists then
            User_warning.emit ~is_error
              ~hints:
                [ Pp.text
                    "generate the project file with: $ dune init project <name>"
                ]
              [ Pp.text
                  "No dune-project file has been found. A default one is \
                   assumed but the project might break when dune is upgraded. \
                   Please create a dune-project file."
              ])
    in
    fun inp ->
      match !Clflags.on_missing_dune_project_file with
      | Ignore -> Memo.return ()
      | Warn -> Memo.exec memo (`Is_error false, inp)
      | Error -> Memo.exec memo (`Is_error true, inp)

  let dune_file ~(dir_status : Sub_dirs.Status.t) ~path ~files ~project =
    let file_exists =
      if dir_status = Data_only then None
      else if
        Dune_project.accept_alternative_dune_file_name project
        && String.Set.mem files Dune_file.alternative_fname
      then Some Dune_file.alternative_fname
      else if String.Set.mem files Dune_file.fname then Some Dune_file.fname
      else None
    in
    let* from_parent =
      match Path.Source.parent path with
      | None -> Memo.return None
      | Some parent ->
        let+ parent = find_dir parent in
        let open Option.O in
        let* parent = parent in
        let* dune_file = parent.contents.dune_file in
        let dir_basename = Path.Source.basename path in
        let+ dir_map =
          Sub_dirs.Dir_map.descend dune_file.plain.for_subdirs dir_basename
        in
        (dune_file.path, dir_map)
    in
    let file =
      match (file_exists, from_parent) with
      | None, None -> None
      | Some fname, _ -> Some (Path.Source.relative path fname)
      | None, Some (path, _) -> Some path
    in
    Memo.Option.map file ~f:(fun file ->
        let open Memo.O in
        let* () = ensure_dune_project_file_exists project in
        let file_exists = Option.is_some file_exists in
        let from_parent = Option.map from_parent ~f:snd in
        Dune_file.load file ~file_exists ~project ~from_parent)

  let contents { Readdir.path; dirs; files } ~dirs_visited ~project
      ~(dir_status : Sub_dirs.Status.t) =
    let+ dune_file = dune_file ~dir_status ~files ~project ~path in
    let sub_dirs = Dune_file.sub_dirs dune_file in
    let dirs_visited, sub_dirs =
      Get_subdir.all ~dirs_visited ~dirs ~sub_dirs ~parent_status:dir_status
        ~dune_file ~path
    in
    (Dir0.Contents.create ~files ~sub_dirs ~dune_file, dirs_visited)

  let get_vcs ~default:vcs ~readdir:{ Readdir.path; files; dirs } =
    match
      Vcs.Kind.of_dir_contents
        (String.Set.union files
           (String.Set.of_list_map dirs ~f:(fun (name, _, _) -> name)))
    with
    | None -> vcs
    | Some kind -> Dir0.This { Vcs.kind; root = Path.(append_source root) path }

  let root () =
    let path = Path.Source.root in
    let dir_status : Sub_dirs.Status.t = Normal in
    let error_unable_to_load ~path unix_error =
      User_error.raise
        [ Pp.textf "Unable to load source %s."
            (Path.Source.to_string_maybe_quoted path)
        ; Unix_error.Detailed.pp ~prefix:"Reason: " unix_error
        ]
    in
    let* readdir =
      Readdir.of_source_path path >>| function
      | Ok dir -> dir
      | Error unix_error -> error_unable_to_load ~path unix_error
    in
    let* project =
      Dune_project.load ~dir:path ~files:readdir.files
        ~infer_from_opam_files:true ~dir_status
      >>| function
      | None -> Dune_project.anonymous ~dir:path ()
      | Some p -> p
    in
    let* readdir = Readdir.filter_files readdir project in
    let vcs = get_vcs ~default:Ancestor_vcs ~readdir in
    let* dirs_visited =
      File.of_source_path (In_source_dir path) >>| function
      | Ok file -> Dirs_visited.singleton path file
      | Error unix_error -> error_unable_to_load ~path unix_error
    in
    let+ contents, visited =
      contents readdir ~dirs_visited ~project ~dir_status
    in
    let dir = Dir0.create ~project ~path ~status:dir_status ~contents ~vcs in
    { Output.dir; visited }

  let find_dir_raw_impl path : Dir0.t Output.t option Memo.t =
    match Path.Source.parent path with
    | None ->
      let+ root = root () in
      Some root
    | Some parent_dir -> (
      let* parent = Memo.Cell.read (find_dir_raw parent_dir) in
      match
        let open Option.O in
        let* { Output.dir = parent_dir; visited = dirs_visited } = parent in
        let* dir_status, virtual_ =
          let basename = Path.Source.basename path in
          let+ sub_dir =
            String.Map.find parent_dir.contents.sub_dirs basename
          in
          let status =
            let status = sub_dir.sub_dir_status in
            if
              Dune_project.cram parent_dir.project
              && Cram_test.is_cram_suffix basename
            then Sub_dirs.Status.Data_only
            else status
          in
          (status, sub_dir.virtual_)
        in
        Some (parent_dir, dirs_visited, dir_status, virtual_)
      with
      | None -> Memo.return None
      | Some (parent_dir, dirs_visited, dir_status, virtual_) ->
        let dirs_visited = Dirs_visited.Per_fn.find dirs_visited path in
        let* readdir =
          if virtual_ then Memo.return (Readdir.empty path)
          else
            Readdir.of_source_path path >>| function
            | Ok dir -> dir
            | Error _ -> Readdir.empty path
        in
        let* project =
          if dir_status = Data_only then Memo.return parent_dir.project
          else
            let+ project =
              Dune_project.load ~dir:path ~files:readdir.files
                ~infer_from_opam_files:false ~dir_status
            in
            Option.value project ~default:parent_dir.project
        in
        let* readdir = Readdir.filter_files readdir project in
        let vcs = get_vcs ~default:parent_dir.vcs ~readdir in
        let* contents, visited =
          contents readdir ~dirs_visited ~project ~dir_status
        in
        let dir =
          Dir0.create ~project ~path ~status:dir_status ~contents ~vcs
        in
        Memo.return (Some { Output.dir; visited }))

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

  let find_dir p =
    Memo.Cell.read (find_dir_raw p) >>| function
    | Some { Output.dir; visited = _ } -> Some dir
    | None -> None

  let root () = find_dir Path.Source.root >>| Option.value_exn
end

let root () = Memoized.root ()

let find_dir path = Memoized.find_dir path

let rec nearest_dir t = function
  | [] -> Memo.return t
  | comp :: components -> (
    match String.Map.find (Dir0.sub_dirs t) comp with
    | None -> Memo.return t
    | Some sub_dir ->
      let* sub_dir = Dir0.sub_dir_as_t sub_dir in
      nearest_dir sub_dir components)

let nearest_dir path =
  let components = Path.Source.explode path in
  let* root = root () in
  nearest_dir root components

let nearest_vcs path =
  let* dir = nearest_dir path in
  match Dir0.vcs dir with
  | This vcs -> Memo.return (Some vcs)
  | Ancestor_vcs -> Memo.Lazy.force ancestor_vcs

let files_of path =
  find_dir path >>| function
  | None -> Path.Source.Set.empty
  | Some dir ->
    Dir0.files dir |> String.Set.to_list
    |> Path.Source.Set.of_list_map ~f:(Path.Source.relative path)

let file_exists path =
  find_dir (Path.Source.parent_exn path) >>| function
  | None -> false
  | Some dir -> String.Set.mem (Dir0.files dir) (Path.Source.basename path)

let dir_exists path = find_dir path >>| Option.is_some

module Dir = struct
  include Dir0

  module Make_map_reduce (M : Memo.S) (Outcome : Monoid) = struct
    open M.O

    let rec map_reduce t ~traverse ~f =
      let must_traverse = Sub_dirs.Status.Map.find traverse t.status in
      match must_traverse with
      | false -> M.return Outcome.empty
      | true ->
        let+ here = f t
        and+ in_sub_dirs =
          M.List.map (String.Map.values t.contents.sub_dirs) ~f:(fun s ->
              let* t = M.of_memo (sub_dir_as_t s) in
              map_reduce t ~traverse ~f)
        in
        List.fold_left in_sub_dirs ~init:here ~f:Outcome.combine
  end

  let cram_tests (t : t) =
    match Dune_project.cram t.project with
    | false -> Memo.return []
    | true ->
      let file_tests =
        String.Set.to_list t.contents.files
        |> List.filter_map ~f:(fun s ->
               if Cram_test.is_cram_suffix s then
                 Some (Ok (Cram_test.File (Path.Source.relative t.path s)))
               else None)
      in
      let+ dir_tests =
        Memo.parallel_map (String.Map.to_list t.contents.sub_dirs)
          ~f:(fun (name, sub_dir) ->
            match Cram_test.is_cram_suffix name with
            | false -> Memo.return None
            | true ->
              let+ t =
                Memo.Cell.read sub_dir.sub_dir_as_t >>| Option.value_exn
              in
              let contents = t.dir in
              let dir = contents.path in
              let fname = Cram_test.fname_in_dir_test in
              let test =
                let file = Path.Source.relative dir fname in
                Cram_test.Dir { file; dir }
              in
              let files = contents.contents.files in
              if String.Set.is_empty files then None
              else
                Some
                  (if String.Set.mem files fname then Ok test
                  else Error (Missing_run_t test)))
        >>| List.filter_opt
      in
      file_tests @ dir_tests
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
end

let is_vendored dir =
  find_dir dir >>| function
  | None -> false
  | Some d -> Dir.status d = Vendored
