open! Stdune
open Import

module File = struct
  type t =
    { ino : int
    ; dev : int
    }

  let to_dyn { ino; dev } =
    let open Dyn.Encoder in
    record [ ("ino", Int.to_dyn ino); ("dev", Int.to_dyn dev) ]

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
      ; sub_dirs : Predicate_lang.Glob.t Sub_dirs.Status.Map.t
      ; mutable sexps : Dune_lang.Ast.t list
      }

    let get_sexp_and_destroy t =
      let sexps = t.sexps in
      t.sexps <- [];
      sexps
  end

  let fname = "dune"

  let jbuild_fname = "jbuild"

  type t =
    | Plain of Plain.t
    | Ocaml_script of Path.Source.t

  let sub_dirs = function
    | Some (Plain p) -> p.sub_dirs
    | None
    | Some (Ocaml_script _) ->
      Sub_dirs.default

  let path = function
    | Plain x -> x.path
    | Ocaml_script p -> p

  let load file ~project =
    Io.with_lexbuf_from_file (Path.source file) ~f:(fun lb ->
        if Dune_lexer.is_script lb then
          Ocaml_script file
        else
          let sexps = Dune_lang.Parser.parse lb ~mode:Many in
          let decoder =
            Dune_project.set_parsing_context project Sub_dirs.decode
          in
          let sub_dirs, sexps =
            Dune_lang.Decoder.parse decoder Univ_map.empty
              (Dune_lang.Ast.List (Loc.none, sexps))
          in
          Plain { path = file; sexps; sub_dirs })
end

module Readdir : sig
  type t = private
    { files : String.Set.t
    ; dirs : (string * Path.Source.t * File.t) list
    }

  val empty : t

  val of_source_path : Path.Source.t -> (t, Unix.error) Result.t
end = struct
  type t =
    { files : String.Set.t
    ; dirs : (string * Path.Source.t * File.t) list
    }

  let empty = { files = String.Set.empty; dirs = [] }

  let _to_dyn { files; dirs } =
    let open Dyn.Encoder in
    record
      [ ("files", String.Set.to_dyn files)
      ; ("dirs", list (triple string Path.Source.to_dyn File.to_dyn) dirs)
      ]

  let is_temp_file fn =
    String.is_prefix fn ~prefix:".#"
    || String.is_suffix fn ~suffix:".swp"
    || String.is_suffix fn ~suffix:"~"

  let of_source_path path =
    match Path.readdir_unsorted (Path.source path) with
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
          List.sort dirs ~compare:(fun (a, _, _) (b, _, _) ->
              String.compare a b)
      }
      |> Result.ok
end

module Output = struct
  type 'a t =
    { dir : 'a
    ; visited : Path.Source.t File.Map.t String.Map.t
    }

  let to_dyn f { dir; visited } =
    let open Dyn.Encoder in
    record
      [ ("dir", f dir)
      ; ( "visited"
        , String.Map.to_dyn (File.Map.to_dyn Path.Source.to_dyn) visited )
      ]
end

module Dir0 = struct
  type t =
    { path : Path.Source.t
    ; status : Sub_dirs.Status.t
    ; contents : contents
    ; project : Dune_project.t
    ; vcs : Vcs.t option
    }

  and contents =
    { files : String.Set.t
    ; sub_dirs : sub_dir String.Map.t
    ; dune_file : Dune_file.t option
    }

  and sub_dir =
    { sub_dir_status : Sub_dirs.Status.t
    ; sub_dir_as_t :
        ( Path.Source.t
        , t Output.t option
        , Path.Source.t -> t Output.t option )
        Memo.Cell.t
    }

  let rec to_dyn { path; status; contents; project = _; vcs } =
    let open Dyn in
    Record
      [ ("path", Path.Source.to_dyn path)
      ; ("status", Sub_dirs.Status.to_dyn status)
      ; ("contents", dyn_of_contents contents)
      ; ("vcs", Dyn.Encoder.option Vcs.to_dyn vcs)
      ]

  and dyn_of_sub_dir { sub_dir_status; sub_dir_as_t } =
    let open Dyn.Encoder in
    let path = Memo.Cell.input sub_dir_as_t in
    record
      [ ("status", Sub_dirs.Status.to_dyn sub_dir_status)
      ; ("sub_dir_as_t", Path.Source.to_dyn path)
      ]

  and dyn_of_contents { files; sub_dirs; dune_file } =
    let open Dyn.Encoder in
    record
      [ ("files", String.Set.to_dyn files)
      ; ("sub_dirs", String.Map.to_dyn dyn_of_sub_dir sub_dirs)
      ; ("dune_file", Dyn.Encoder.(option opaque dune_file))
      ; ("project", Dyn.opaque)
      ]

  module Contents = struct
    let create ~files ~sub_dirs ~dune_file = { files; sub_dirs; dune_file }
  end

  let create ~project ~path ~status ~contents ~vcs =
    { path; status; contents; project; vcs }

  let contents t = t.contents

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
    String.Map.foldi (sub_dirs t) ~init:Path.Source.Set.empty ~f:(fun s _ acc ->
        Path.Source.Set.add acc (Path.Source.relative t.path s))
end

module Settings : sig
  type t =
    { ancestor_vcs : Vcs.t option
    ; recognize_jbuilder_projects : bool
    }

  val set : t -> unit

  val get : unit -> t
end = struct
  type t =
    { ancestor_vcs : Vcs.t option
    ; recognize_jbuilder_projects : bool
    }

  let equal { ancestor_vcs; recognize_jbuilder_projects } y =
    Option.equal Vcs.equal ancestor_vcs y.ancestor_vcs
    && Bool.equal recognize_jbuilder_projects y.recognize_jbuilder_projects

  let to_dyn { ancestor_vcs; recognize_jbuilder_projects } =
    let open Dyn.Encoder in
    record
      [ ("ancestor_vcs", option Vcs.to_dyn ancestor_vcs)
      ; ("recognize_jbuilder_projects", bool recognize_jbuilder_projects)
      ]

  let t = Fdecl.create to_dyn

  let set x =
    match Fdecl.peek t with
    | None -> Fdecl.set t x
    | Some x' ->
      if not (equal x x') then
        (* The next call will fail, but will give a good error message *)
        Fdecl.set t x

  let get () =
    let (_ : Memo.Run.t) = Memo.current_run () in
    Fdecl.get t
end

let init ~ancestor_vcs ~recognize_jbuilder_projects =
  Settings.set { ancestor_vcs; recognize_jbuilder_projects }

module rec Memoized : sig
  val root : unit -> Dir0.t

  (* Not part of the interface. Only necessary to call recursively *)
  val find_dir_raw :
       Path.Source.t
    -> ( Path.Source.t
       , Dir0.t Output.t option
       , Path.Source.t -> Dir0.t Output.t option )
       Memo.Cell.t

  val find_dir : Path.Source.t -> Dir0.t option
end = struct
  open Memoized

  let get_sub_dirs ~dirs_visited ~dirs ~sub_dirs
      ~(dir_status : Sub_dirs.Status.t) =
    let sub_dirs =
      Sub_dirs.eval sub_dirs ~dirs:(List.map ~f:(fun (a, _, _) -> a) dirs)
    in
    dirs
    |> List.fold_left ~init:(String.Map.empty, String.Map.empty)
         ~f:(fun (dirs_visited_acc, subdirs) (fn, path, file) ->
           let status = Sub_dirs.status sub_dirs ~dir:fn in
           match status with
           | Ignored -> (dirs_visited_acc, subdirs)
           | Status status ->
             let dir_status : Sub_dirs.Status.t =
               match (dir_status, status) with
               | Data_only, _ -> Data_only
               | Vendored, Normal -> Vendored
               | _, _ -> status
             in
             let dirs_visited_acc =
               if Sys.win32 then
                 dirs_visited_acc
               else
                 let new_dirs_visited =
                   File.Map.update dirs_visited file ~f:(function
                     | None -> Some path
                     | Some first_path ->
                       User_error.raise
                         [ Pp.textf
                             "Path %s has already been scanned. Cannot scan it \
                              again through symlink %s"
                             (Path.Source.to_string_maybe_quoted first_path)
                             (Path.Source.to_string_maybe_quoted path)
                         ])
                 in
                 String.Map.add_exn dirs_visited_acc fn new_dirs_visited
             in
             let sub_dir =
               let sub_dir_as_t = find_dir_raw path in
               { Dir0.sub_dir_status = dir_status; sub_dir_as_t }
             in
             let subdirs = String.Map.set subdirs fn sub_dir in
             (dirs_visited_acc, subdirs))

  let contents { Readdir.dirs; files } ~dirs_visited ~project ~path
      ~(dir_status : Sub_dirs.Status.t) =
    let recognize_jbuilder_projects =
      let settings = Settings.get () in
      settings.recognize_jbuilder_projects
    in
    let dune_file =
      if dir_status = Data_only then
        None
      else if
        (not recognize_jbuilder_projects)
        && String.Set.mem files Dune_file.jbuild_fname
      then
        User_error.raise
          ~loc:
            (Loc.in_file
               (Path.source (Path.Source.relative path Dune_file.jbuild_fname)))
          [ Pp.text
              "jbuild files are no longer supported, please convert this file \
               to a dune file instead."
          ; Pp.text
              "Note: You can use \"dune upgrade\" to convert your project to \
               dune."
          ]
      else if not (String.Set.mem files Dune_file.fname) then
        None
      else (
        ignore
          ( Dune_project.ensure_project_file_exists project
            : Dune_project.created_or_already_exist );
        let file = Path.Source.relative path Dune_file.fname in
        Some (Dune_file.load file ~project)
      )
    in
    let sub_dirs = Dune_file.sub_dirs dune_file in
    let dirs_visited, sub_dirs =
      get_sub_dirs ~dirs_visited ~dirs ~sub_dirs ~dir_status
    in
    (Dir0.Contents.create ~files ~sub_dirs ~dune_file, dirs_visited)

  let root () =
    let settings = Settings.get () in
    let path = Path.Source.root in
    let dir_status : Sub_dirs.Status.t = Normal in
    let readdir =
      match Readdir.of_source_path path with
      | Ok dir -> dir
      | Error m ->
        User_error.raise
          [ Pp.textf "Unable to load source %s.@.Reason:%s@."
              (Path.Source.to_string_maybe_quoted path)
              (Unix.error_message m)
          ]
    in
    let project =
      match
        Dune_project.load ~dir:path ~files:readdir.files
          ~infer_from_opam_files:true
      with
      | None -> Dune_project.anonymous ~dir:path
      | Some p -> p
    in
    let vcs = settings.ancestor_vcs in
    let dirs_visited = File.Map.singleton (File.of_source_path path) path in
    let contents, visited =
      contents readdir ~dirs_visited ~project ~path ~dir_status
    in
    let dir = Dir0.create ~project ~path ~status:dir_status ~contents ~vcs in
    { Output.dir; visited }

  let get_vcs ~default:vcs ~path ~readdir:{ Readdir.files; dirs } =
    match
      match
        List.find_map dirs ~f:(fun (name, _, _) -> Vcs.Kind.of_filename name)
      with
      | Some kind -> Some kind
      | None -> Vcs.Kind.of_dir_contents files
    with
    | None -> vcs
    | Some kind -> Some { Vcs.kind; root = Path.(append_source root) path }

  let find_dir_raw_impl path : Dir0.t Output.t option =
    match Path.Source.parent path with
    | None -> Some (root ())
    | Some parent_dir ->
      let open Option.O in
      let* { Output.dir = parent_dir; visited = dirs_visited } =
        Memo.Cell.get_sync (find_dir_raw parent_dir)
      in
      let* dir_status =
        let basename = Path.Source.basename path in
        let+ sub_dir = String.Map.find parent_dir.contents.sub_dirs basename in
        sub_dir.sub_dir_status
      in
      let dirs_visited =
        String.Map.find dirs_visited (Path.Source.basename path)
        |> Option.value ~default:File.Map.empty
      in
      let settings = Settings.get () in
      let readdir =
        match Readdir.of_source_path path with
        | Ok dir -> dir
        | Error _ -> Readdir.empty
      in
      let project =
        if dir_status = Data_only then
          parent_dir.project
        else
          Option.value
            (Dune_project.load ~dir:path ~files:readdir.files
               ~infer_from_opam_files:settings.recognize_jbuilder_projects)
            ~default:parent_dir.project
      in
      let vcs = get_vcs ~default:parent_dir.vcs ~readdir ~path in
      let contents, visited =
        contents readdir ~dirs_visited ~project ~path ~dir_status
      in
      let dir = Dir0.create ~project ~path ~status:dir_status ~contents ~vcs in
      Some { Output.dir; visited }

  let find_dir_raw =
    let module Output = struct
      type t = Dir0.t Output.t option

      let to_dyn =
        let open Dyn.Encoder in
        option (Output.to_dyn Dir0.to_dyn)
    end in
    let memo =
      Memo.create "find-dir-raw" ~doc:"get file tree"
        ~input:(module Path.Source)
        ~output:(Simple (module Output))
        ~visibility:Memo.Visibility.Hidden Sync find_dir_raw_impl
    in
    Memo.cell memo

  let find_dir p =
    let open Option.O in
    let+ { Output.dir; visited = _ } = Memo.Cell.get_sync (find_dir_raw p) in
    dir

  let root () = Option.value_exn (find_dir Path.Source.root)
end

let root () = Memoized.root ()

let find_dir path = Memoized.find_dir path

let rec nearest_dir t = function
  | [] -> t
  | comp :: components -> (
    match String.Map.find (Dir0.sub_dirs t) comp with
    | None -> t
    | Some _ ->
      let path = Path.Source.relative (Dir0.path t) comp in
      let dir = Option.value_exn (find_dir path) in
      nearest_dir dir components )

let nearest_dir path =
  let components = Path.Source.explode path in
  nearest_dir (root ()) components

let nearest_vcs path = Dir0.vcs (nearest_dir path)

let files_of path =
  match find_dir path with
  | None -> Path.Source.Set.empty
  | Some dir ->
    Path.Source.Set.of_list
      (List.map
         (String.Set.to_list (Dir0.files dir))
         ~f:(Path.Source.relative path))

let file_exists path =
  match find_dir (Path.Source.parent_exn path) with
  | None -> false
  | Some dir -> String.Set.mem (Dir0.files dir) (Path.Source.basename path)

let dir_exists path = Option.is_some (find_dir path)

let dir_is_vendored path =
  Option.map ~f:(fun dir -> Dir0.vendored dir) (find_dir path)

module Dir = struct
  include Dir0

  let sub_dir_as_t (s : sub_dir) =
    (Memo.Cell.get_sync s.sub_dir_as_t |> Option.value_exn).dir

  let fold_sub_dirs (t : t) ~init ~f =
    String.Map.foldi t.contents.sub_dirs ~init ~f:(fun name s acc ->
        f name (sub_dir_as_t s) acc)

  let rec fold t ~traverse ~init:acc ~f =
    let must_traverse = Sub_dirs.Status.Map.find traverse t.status in
    match must_traverse with
    | false -> acc
    | true ->
      let acc = f t acc in
      fold_sub_dirs t ~init:acc ~f:(fun _name t acc ->
          fold t ~traverse ~init:acc ~f)
end

let fold_with_progress ~traverse ~init ~f =
  let root = root () in
  let nb_path_visited = ref 0 in
  Console.Status_line.set (fun () ->
      Some (Pp.textf "Scanned %i directories" !nb_path_visited));
  let res =
    Dir.fold root ~traverse ~init ~f:(fun dir acc ->
        incr nb_path_visited;
        if !nb_path_visited mod 100 = 0 then Console.Status_line.refresh ();
        f dir acc)
  in
  Console.Status_line.set (Fn.const None);
  res
