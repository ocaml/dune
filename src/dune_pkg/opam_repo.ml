open Import
open Fiber.O

module Paths = struct
  let packages = Path.Local.of_string "packages"
  let repo = Path.Local.of_string "repo"

  let package_root package_name =
    OpamPackage.Name.to_string package_name |> Path.Local.relative packages
  ;;

  let package_dir package =
    Path.Local.relative
      (package_root (OpamPackage.name package))
      (OpamPackage.to_string package)
  ;;

  let files_dir package = Path.Local.relative (package_dir package) "files"
  let opam_file package = Path.Local.relative (package_dir package) "opam"
end

let has_url_scheme url =
  match String.index url ':' with
  | None -> false
  | Some colon ->
    colon + 2 < String.length url
    && Char.equal url.[colon + 1] '/'
    && Char.equal url.[colon + 2] '/'
;;

let repository_url_for_relative_mirrors (url : OpamUrl.t) =
  let backend =
    match url.transport with
    | "http" | "https" -> `http
    | "file" -> `rsync
    | _ -> url.backend
  in
  { url with backend; hash = None }
;;

let archive_mirrors_of_repo_file ~loc ~repository_url contents =
  match OpamFile.Repo.read_from_string contents with
  | exception
      ((OpamPp.Bad_format _ | OpamPp.Bad_format_list _ | OpamPp.Bad_version _) as exn) ->
    User_warning.emit
      ~loc
      [ Pp.text "Ignoring archive mirrors from invalid opam repository metadata."
      ; Pp.text (OpamPp.string_of_bad_format exn)
      ];
    []
  | repo ->
    OpamFile.Repo.dl_cache repo
    |> List.filter_map ~f:(fun archive_mirror ->
      if has_url_scheme archive_mirror
      then (
        match OpamUrl.of_string archive_mirror with
        | archive_mirror -> Some archive_mirror
        | exception OpamUrl.Parse_error message ->
          User_warning.emit
            ~loc
            [ Pp.textf "Ignoring invalid opam archive mirror %S." archive_mirror
            ; Pp.text message
            ];
          None)
      else (
        (* Relative mirrors resolve against the repository URL. A git URL
           addresses a remote, not a directory dune can read from, so such
           entries are skipped. The upstream opam repository declares a
           relative "cache" aimed at opam's HTTP mirror; warning here would
           fire on every solve. *)
        match (repository_url : OpamUrl.t).backend with
        | `git -> None
        | `http | `rsync | `hg | `darcs ->
          Some
            (OpamUrl.append
               (repository_url_for_relative_mirrors repository_url)
               archive_mirror)))
    |> List.filter ~f:(fun archive_mirror ->
      let supported = OpamUrl.is_supported_archive_mirror archive_mirror in
      if not supported
      then
        User_warning.emit
          ~loc
          [ Pp.textf
              "Ignoring unsupported opam archive mirror %s."
              (OpamUrl.to_string archive_mirror)
          ];
      supported)
    |> OpamUrl.dedup_preserving_order
;;

let archive_mirrors_from_directory ~loc ~repository_url dir =
  let repo_file = Path.append_local dir Paths.repo in
  match Io.read_file repo_file with
  | contents -> archive_mirrors_of_repo_file ~loc ~repository_url contents
  | exception Unix.Unix_error (ENOENT, _, _) -> []
;;

module Serializable = struct
  type t = string

  let equal = String.equal

  let to_dyn source =
    let open Dyn in
    variant "opam_repo_serializable" [ string source ]
  ;;

  let encode source =
    let open Encoder in
    record_fields [ field "source" string source ]
  ;;

  let decode =
    let open Decoder in
    fields
      (let+ source = field "source" string in
       source)
  ;;
end

type t =
  { source : Source_backend.t
  ; loc : Loc.t
  ; serializable : Serializable.t option
  ; archive_mirrors : OpamUrl.t list
  }

let to_dyn { source; loc; serializable; archive_mirrors } =
  Dyn.record
    [ "source", Source_backend.to_dyn source
    ; "loc", Loc.to_dyn loc
    ; "serializable", Dyn.option Serializable.to_dyn serializable
    ; "archive_mirrors", Dyn.list OpamUrl.to_dyn archive_mirrors
    ]
;;

let equal { source; serializable; loc; archive_mirrors } t =
  Source_backend.equal source t.source
  && Option.equal Serializable.equal serializable t.serializable
  && Loc.equal loc t.loc
  && List.equal OpamUrl.equal archive_mirrors t.archive_mirrors
;;

let serializable { serializable; _ } = serializable
let archive_mirrors { archive_mirrors; _ } = archive_mirrors

let of_opam_repo_dir_path_with_archive_mirrors
      loc
      opam_repo_dir_path
      initial_archive_mirrors
  =
  (match Path.stat opam_repo_dir_path with
   | Error (Unix.ENOENT, _, _) ->
     User_error.raise
       ~loc
       [ Pp.textf "%s does not exist" (Path.to_string_maybe_quoted opam_repo_dir_path) ]
   | Error _ ->
     User_error.raise
       ~loc
       [ Pp.textf "could not read %s" (Path.to_string_maybe_quoted opam_repo_dir_path) ]
   | Ok { Unix.st_kind = S_DIR; _ } -> ()
   | Ok _ ->
     User_error.raise
       ~loc
       [ Pp.textf "%s is not a directory" (Path.to_string_maybe_quoted opam_repo_dir_path)
       ]);
  (let packages = Path.append_local opam_repo_dir_path Paths.packages in
   match Path.stat packages with
   | Ok { Unix.st_kind = S_DIR; _ } -> ()
   | Ok _ ->
     User_error.raise
       ~loc
       [ Pp.textf "%s is not a directory" (Path.to_string_maybe_quoted packages) ]
   | Error (Unix.ENOENT, _, _) ->
     User_error.raise
       ~loc
       [ Pp.textf
           "%s doesn't look like a path to an opam repository as it lacks a subdirectory \
            named \"packages\""
           (Path.to_string_maybe_quoted opam_repo_dir_path)
       ]
   | Error _ ->
     User_error.raise
       ~loc
       [ Pp.textf "could not read %s" (Path.to_string_maybe_quoted opam_repo_dir_path) ]);
  let repository_url : OpamUrl.t =
    { transport = "file"
    ; path = Path.to_string opam_repo_dir_path
    ; hash = None
    ; backend = `rsync
    }
  in
  let archive_mirrors =
    archive_mirrors_from_directory ~loc ~repository_url opam_repo_dir_path
  in
  { source = Directory opam_repo_dir_path
  ; serializable = None
  ; loc
  ; archive_mirrors =
      OpamUrl.dedup_preserving_order (initial_archive_mirrors @ archive_mirrors)
  }
;;

let of_opam_repo_dir_path loc opam_repo_dir_path =
  of_opam_repo_dir_path_with_archive_mirrors loc opam_repo_dir_path []
;;

let of_git_repo_with_archive_mirrors loc url initial_archive_mirrors =
  let* at_rev =
    let* rev_store = Rev_store.get in
    OpamUrl.resolve url ~loc rev_store
    >>= (function
     | Error _ as e -> Fiber.return e
     | Ok s -> OpamUrl.fetch_revision url ~loc s rev_store)
    >>| User_error.ok_exn
  in
  let+ repo_file = Rev_store.At_rev.content at_rev Paths.repo in
  let serializable =
    Some
      (sprintf
         "%s#%s"
         (OpamUrl.base_url url)
         (Rev_store.Object.to_hex (Rev_store.At_rev.rev at_rev))
       |> OpamUrl.of_string
       |> OpamUrl.to_string)
  in
  let archive_mirrors =
    match repo_file with
    | None -> []
    | Some contents -> archive_mirrors_of_repo_file ~loc ~repository_url:url contents
  in
  { source = Repo at_rev
  ; serializable
  ; loc
  ; archive_mirrors =
      OpamUrl.dedup_preserving_order (initial_archive_mirrors @ archive_mirrors)
  }
;;

let of_git_repo loc url = of_git_repo_with_archive_mirrors loc url []

let resolve_repositories ~available_repos ~repositories =
  repositories
  |> Fiber.parallel_map ~f:(fun (loc, name) ->
    match Workspace.Repository.Name.Map.find available_repos name with
    | None ->
      User_error.raise
        ~loc
        [ Pp.textf
            "Repository '%s' is not a known repository"
            (Workspace.Repository.Name.to_string name)
        ]
    | Some repo ->
      let loc, opam_url = Workspace.Repository.opam_url repo in
      let archive_mirrors = Workspace.Repository.archive_mirrors repo in
      (match OpamUrl.classify opam_url loc with
       | `Git -> of_git_repo_with_archive_mirrors loc opam_url archive_mirrors
       | `Path path ->
         Fiber.return
         @@ of_opam_repo_dir_path_with_archive_mirrors loc path archive_mirrors
       | `Archive ->
         User_error.raise
           ~loc
           [ Pp.textf
               "Repositories stored in archives (%s) are currently unsupported"
               (OpamUrl.to_string opam_url)
           ]))
;;

let revision t =
  match t.source with
  | Repo r -> r
  | Directory _ -> Code_error.raise "not a git repo" []
;;

let content_digest t =
  match t.source with
  | Repo repo ->
    Rev_store.At_rev.rev repo |> Rev_store.Object.to_hex |> Dune_digest.string
  | Directory path -> Path_digest.digest_with_lstat path
;;

let load_opam_package_from_dir ~(dir : Path.t) ~archive_mirrors package =
  let opam_file_path = Path.append_local dir (Paths.opam_file package) in
  match Fpath.exists (Path.to_string opam_file_path) with
  | false -> None
  | true ->
    let files_dir = Some (Paths.files_dir package) in
    let opam_file =
      let loc = Loc.in_file opam_file_path in
      loc, Opam_file.opam_file_of_path opam_file_path
    in
    Some
      (Resolved_package.local_fs
         package
         opam_file
         ~dir
         ~files_dir
         ~url:None
         ~archive_mirrors)
;;

let load_packages_from_git rev_store opam_packages =
  let+ contents =
    List.map opam_packages ~f:(fun (file, _, _, _, _) -> file)
    |> Rev_store.content_of_files rev_store
  in
  List.map2
    opam_packages
    contents
    ~f:(fun (opam_file, package, rev, files_dir, archive_mirrors) contents ->
      let opam_file =
        let path = opam_file |> Rev_store.File.path |> Path.of_local in
        let loc = Loc.in_file path in
        loc, Opam_file.opam_file_of_string_exn ~contents path
      in
      Resolved_package.git_repo
        package
        opam_file
        rev
        ~files_dir:(Some files_dir)
        ~url:None
        ~archive_mirrors)
;;

let all_packages_in_dir_at_path ~dir ~path loc =
  let dir = Path.append_local dir path in
  match Path.readdir_unsorted dir with
  | Ok version_dirs -> version_dirs
  | Error (Unix.ENOENT, _, _) -> []
  | Error e ->
    let err =
      if Path.Local.(path <> Paths.packages) then "package versions" else "packages"
    in
    User_error.raise
      ~loc
      [ Pp.textf
          "Unable to read %s from %s: %s"
          err
          (Path.to_string_maybe_quoted dir)
          (Unix_error.Detailed.to_string_hum e)
      ]
;;

let all_packages_versions_in_dir loc ~dir opam_package_name =
  let path = Paths.package_root opam_package_name in
  all_packages_in_dir_at_path ~dir ~path loc
  |> List.map ~f:(fun name -> OpamPackage.of_string (Filename.to_string name))
;;

let all_packages_versions_at_rev_at_path ~path rev =
  Rev_store.At_rev.directory_entries rev ~recursive:true path
  |> Rev_store.File.Set.to_list
  |> List.filter_map ~f:(fun file ->
    let path = Rev_store.File.path file in
    let open Option.O in
    Path.Local.basename_opt path
    >>= fun basename ->
    match Filename.to_string basename with
    | "opam" ->
      let+ package =
        Path.Local.parent path
        >>| Path.Local.basename
        >>| Filename.to_string
        >>| OpamPackage.of_string
      in
      file, package
    | _ -> None)
;;

let all_packages_versions_at_rev rev opam_package_name =
  let path = Paths.package_root opam_package_name in
  all_packages_versions_at_rev_at_path ~path rev
;;

module Key = struct
  type t =
    | Directory of OpamPackage.t
    | Git of Rev_store.File.t * OpamPackage.t * Rev_store.At_rev.t * Path.Local.t

  let opam_package = function
    | Directory p | Git (_, p, _, _) -> p
  ;;
end

let all_package_versions t opam_package_name : Key.t list =
  match t.source with
  | Directory dir ->
    all_packages_versions_in_dir t.loc ~dir opam_package_name
    |> List.map ~f:(fun pkg -> Key.Directory pkg)
  | Repo rev ->
    all_packages_versions_at_rev rev opam_package_name
    |> List.map ~f:(fun (file, pkg) ->
      let files_dir = Paths.files_dir pkg in
      Key.Git (file, pkg, rev, files_dir))
;;

let all_packages_versions_map ts opam_package_name =
  List.concat_map ts ~f:(fun t ->
    all_package_versions t opam_package_name |> List.rev_map ~f:(fun pkg -> t, pkg))
  |> List.fold_left ~init:OpamPackage.Version.Map.empty ~f:(fun acc (repo, pkg) ->
    let version =
      let pkg = Key.opam_package pkg in
      OpamPackage.version pkg
    in
    if OpamPackage.Version.Map.mem version acc
    then acc
    else OpamPackage.Version.Map.add version (repo, pkg) acc)
;;

let load_all_versions_by_keys ts =
  let from_git, from_dirs =
    OpamPackage.Version.Map.values ts
    |> List.partition_map ~f:(fun (repo, (pkg : Key.t)) ->
      match pkg with
      | Git (file, pkg, rev, files_dir) ->
        Left (file, pkg, rev, files_dir, repo.archive_mirrors)
      | Directory pkg -> Right (repo, pkg))
  in
  let from_dirs =
    List.filter_map from_dirs ~f:(fun (repo, pkg) ->
      match repo.source with
      | Repo _ ->
        Code_error.raise
          "impossible because all elements in from_dirs are from a directory"
          []
      | Directory dir ->
        load_opam_package_from_dir ~dir ~archive_mirrors:repo.archive_mirrors pkg
        |> Option.map ~f:(fun resolved_package -> pkg, resolved_package))
  in
  let+ from_git =
    match from_git with
    | [] -> Fiber.return []
    | packages ->
      let* rev_store = Rev_store.get in
      let+ resolved_packages = load_packages_from_git rev_store packages in
      List.map2 resolved_packages packages ~f:(fun resolved_package (_, pkg, _, _, _) ->
        pkg, resolved_package)
  in
  from_dirs @ from_git
  |> List.rev_map ~f:(fun (opam_package, resolved_package) ->
    OpamPackage.version opam_package, resolved_package)
  |> OpamPackage.Version.Map.of_list
;;

let load_all_versions ts opam_package_name =
  all_packages_versions_map ts opam_package_name |> load_all_versions_by_keys
;;

let packages_in_repo repo =
  let path = Paths.packages in
  match repo.source with
  | Repo rev ->
    all_packages_versions_at_rev_at_path ~path rev
    |> List.map ~f:(fun (_opam_file, pkg) -> OpamPackage.name pkg)
    |> OpamPackage.Name.Set.of_list
    |> OpamPackage.Name.Set.elements
  | Directory dir ->
    all_packages_in_dir_at_path ~path ~dir repo.loc
    |> List.map ~f:(fun name -> OpamPackage.Name.of_string (Filename.to_string name))
;;

module Private = struct
  let create ~source:serializable =
    let packages_dir_path = Path.of_string "/" in
    { source = Directory packages_dir_path
    ; serializable
    ; loc = Loc.none
    ; archive_mirrors = []
    }
  ;;
end
