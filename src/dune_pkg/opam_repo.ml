open Import
open Fiber.O

let rev_store =
  let store = ref None in
  Fiber.of_thunk (fun () ->
    match !store with
    | Some s -> Fiber.return s
    | None ->
      let dir =
        Path.L.relative
          (Path.of_string (Xdg.cache_dir (Lazy.force Dune_util.xdg)))
          [ "dune"; "git-repo" ]
      in
      let+ rev_store = Rev_store.load_or_create ~dir in
      store := Some rev_store;
      rev_store)
;;

module Paths = struct
  let packages = Path.Local.of_string "packages"

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

module Serializable = struct
  type t =
    { repo_id : Repository_id.t option
    ; source : string
    }

  let equal { repo_id; source } t =
    Option.equal Repository_id.equal repo_id t.repo_id && String.equal source t.source
  ;;

  let to_dyn { repo_id; source } =
    let open Dyn in
    variant
      "opam_repo_serializable"
      [ option Repository_id.to_dyn repo_id; string source ]
  ;;

  let encode { repo_id; source } =
    let open Encoder in
    record_fields
      [ field "source" string source; field_o "repo_id" Repository_id.encode repo_id ]
  ;;

  let decode =
    let open Decoder in
    fields
      (let+ source = field "source" string
       and+ repo_id = field_o "repo_id" Repository_id.decode in
       { repo_id; source })
  ;;
end

module Backend = struct
  type t =
    | Directory of Path.t
    | Repo of Rev_store.At_rev.t

  let equal a b =
    match a, b with
    | Directory a, Directory b -> Path.equal a b
    | Repo a, Repo b -> Rev_store.At_rev.equal a b
    | _, _ -> false
  ;;
end

type t =
  { source : Backend.t
  ; serializable : Serializable.t option
  }

let equal { source; serializable } t =
  Backend.equal source t.source
  && Option.equal Serializable.equal serializable t.serializable
;;

let serializable { serializable; _ } = serializable

let repo_id t =
  let open Option.O in
  let* serializable = serializable t in
  serializable.repo_id
;;

let source t =
  let open Option.O in
  let+ serializable = serializable t in
  serializable.source
;;

let of_opam_repo_dir_path ~source ~repo_id opam_repo_dir_path =
  (match Path.stat opam_repo_dir_path with
   | Error (Unix.ENOENT, _, _) ->
     User_error.raise
       [ Pp.textf "%s does not exist" (Path.to_string_maybe_quoted opam_repo_dir_path) ]
   | Error _ ->
     User_error.raise
       [ Pp.textf "could not read %s" (Path.to_string_maybe_quoted opam_repo_dir_path) ]
   | Ok { Unix.st_kind = S_DIR; _ } -> ()
   | Ok _ ->
     User_error.raise
       [ Pp.textf "%s is not a directory" (Path.to_string_maybe_quoted opam_repo_dir_path)
       ]);
  (let packages = Path.append_local opam_repo_dir_path Paths.packages in
   match Path.stat packages with
   | Ok { Unix.st_kind = S_DIR; _ } -> ()
   | Ok _ ->
     User_error.raise
       [ Pp.textf "%s is not a directory" (Path.to_string_maybe_quoted packages) ]
   | Error (Unix.ENOENT, _, _) ->
     User_error.raise
       [ Pp.textf
           "%s doesn't look like a path to an opam repository as it lacks a subdirectory \
            named \"packages\""
           (Path.to_string_maybe_quoted opam_repo_dir_path)
       ]
   | Error _ ->
     User_error.raise
       [ Pp.textf "could not read %s" (Path.to_string_maybe_quoted opam_repo_dir_path) ]);
  let serializable =
    Option.map source ~f:(fun source -> { Serializable.repo_id; source })
  in
  { source = Directory opam_repo_dir_path; serializable }
;;

let of_git_repo ~repo_id ~source =
  let+ at_rev, computed_repo_id =
    let* remote =
      let* repo = rev_store in
      Rev_store.add_repo repo ~source
    in
    match repo_id with
    | Some repo_id ->
      let+ at_rev = Rev_store.Remote.rev_of_repository_id remote repo_id in
      at_rev, Some repo_id
    | None ->
      let+ at_rev =
        let* name =
          Rev_store.Remote.default_branch remote
          >>| function
          | Some name -> name
          | None ->
            User_error.raise
              ~hints:
                [ Pp.text
                    "Specify a different repository with a default branch or an exiting \
                     revision"
                ]
              [ Pp.textf
                  "No revision given and default branch could not be determined in \
                   repository %s"
                  source
              ]
        in
        Rev_store.Remote.rev_of_name remote ~name
      in
      let repo_id = Option.map at_rev ~f:Rev_store.At_rev.repository_id in
      at_rev, repo_id
  in
  match at_rev with
  | None ->
    User_error.raise
      ~hints:[ Pp.text "Double check that the revision is included in the repository" ]
      [ Pp.textf "Could not find revision in repository %s" source ]
  | Some at_rev ->
    let serializable = Some { Serializable.repo_id = computed_repo_id; source } in
    { source = Repo at_rev; serializable }
;;

let if_exists p =
  match Path.exists p with
  | false -> None
  | true -> Some p
;;

(* Scan a path recursively down retrieving a list of all files together with their
   relative path. *)
let scan_files_entries path =
  (* TODO Add some cycle detection *)
  let rec read acc dir =
    let path = Path.append_local path dir in
    match Path.readdir_unsorted_with_kinds path with
    | Ok entries ->
      List.fold_left entries ~init:acc ~f:(fun acc (filename, kind) ->
        let local_path = Path.Local.relative dir filename in
        match (kind : Unix.file_kind) with
        | S_REG -> local_path :: acc
        | S_DIR -> read acc local_path
        | _ ->
          (* TODO should be an error *)
          acc)
    | Error (Unix.ENOENT, _, _) -> acc
    | Error err ->
      User_error.raise
        ~loc:(Loc.in_file path)
        [ Pp.text "Unable to read file in opam repository:"; Unix_error.Detailed.pp err ]
  in
  read [] Path.Local.root
;;

let get_opam_package_files t opam_package =
  match t.source with
  | Directory path ->
    let file_path = Path.append_local path (Paths.files_dir opam_package) in
    scan_files_entries file_path
    |> List.map ~f:(fun local_file ->
      let original = File_entry.Path (Path.append_local file_path local_file) in
      { File_entry.local_file; original })
    |> Fiber.return
  | Repo at_rev ->
    let files_root = Paths.files_dir opam_package in
    Rev_store.At_rev.directory_entries at_rev files_root
    |> Rev_store.File.Set.to_list
    |> Fiber.parallel_map ~f:(fun remote_file ->
      let remote_file_path = Rev_store.File.path remote_file in
      Rev_store.At_rev.content at_rev remote_file_path
      >>| function
      | None ->
        Code_error.raise
          "Enumerated file in directory but file can't be retrieved"
          [ "local_file", Rev_store.File.to_dyn remote_file ]
      | Some content ->
        let local_file =
          Path.Local.descendant ~of_:files_root remote_file_path |> Option.value_exn
        in
        { File_entry.local_file; original = Content content })
;;

module With_file = struct
  type nonrec t =
    { opam_file : OpamFile.OPAM.t
    ; file : Path.t
    ; repo : t
    }

  let file t = t.file
  let opam_file t = t.opam_file
  let repo t = t.repo
end

let load_opam_package_from_dir ~(dir : Path.t) opam_package =
  Paths.opam_file opam_package
  |> Path.append_local dir
  |> if_exists
  |> Option.map ~f:(fun opam_file_path ->
    let opam_file =
      Path.to_string opam_file_path
      |> OpamFilename.raw
      |> OpamFile.make
      |> OpamFile.OPAM.read
    in
    { With_file.opam_file
    ; file = opam_file_path
    ; repo = { source = Backend.Directory dir; serializable = None }
    })
;;

(* Reads an opam package definition from an "opam" file in this repository
   corresponding to a package (name and version). *)
let load_opam_package t opam_package =
  match t.source with
  | Directory dir -> load_opam_package_from_dir ~dir opam_package |> Fiber.return
  | Repo at_rev ->
    let file = Paths.opam_file opam_package in
    Rev_store.At_rev.content at_rev file
    >>| Option.map ~f:(fun content ->
      let opam_file =
        (* the filename is used to read the version number *)
        let filename =
          Path.Local.to_string file |> OpamFilename.of_string |> OpamFile.make
        in
        OpamFile.OPAM.read_from_string ~filename content
      in
      (* TODO the [file] here is made up *)
      { With_file.opam_file; file = Path.source @@ Path.Source.of_local file; repo = t })
;;

let load_packages_from_git rev_store opam_packages =
  let+ contents = List.map opam_packages ~f:snd |> Rev_store.content_of_files rev_store in
  List.map2 opam_packages contents ~f:(fun (repo, file) opam_file_contents ->
    let path = Rev_store.File.path file in
    let opam_file =
      let filename =
        (* the filename is used to read the version number *)
        Path.Local.to_string path |> OpamFilename.of_string |> OpamFile.make
      in
      OpamFile.OPAM.read_from_string ~filename opam_file_contents
    in
    (* TODO the [file] here is made up *)
    { With_file.opam_file; file = Path.source @@ Path.Source.of_local path; repo })
;;

let all_packages_versions_in_dir ~dir opam_package_name =
  let dir = Path.append_local dir (Paths.package_root opam_package_name) in
  match Path.readdir_unsorted dir with
  | Ok version_dirs -> List.map version_dirs ~f:OpamPackage.of_string
  | Error (Unix.ENOENT, _, _) -> []
  | Error e ->
    User_error.raise
      [ Pp.textf
          "Unable to read package versions from %s: %s"
          (Path.to_string_maybe_quoted dir)
          (Dune_filesystem_stubs.Unix_error.Detailed.to_string_hum e)
      ]
;;

let all_packages_versions_at_rev rev opam_package_name =
  Paths.package_root opam_package_name
  |> Rev_store.At_rev.directory_entries rev
  |> Rev_store.File.Set.to_list
  |> List.filter_map ~f:(fun file ->
    let path = Rev_store.File.path file in
    let open Option.O in
    Path.Local.basename_opt path
    >>= function
    | "opam" ->
      let+ package =
        Path.Local.parent path >>| Path.Local.basename >>| OpamPackage.of_string
      in
      file, package
    | _ -> None)
;;

let all_package_versions t opam_package_name =
  match t.source with
  | Directory dir ->
    all_packages_versions_in_dir ~dir opam_package_name
    |> List.map ~f:(fun pkg -> None, pkg)
  | Repo rev ->
    all_packages_versions_at_rev rev opam_package_name
    |> List.map ~f:(fun (file, pkg) -> Some file, pkg)
;;

let load_all_versions ts opam_package_name =
  let from_git, from_dirs =
    List.map ts ~f:(fun t ->
      all_package_versions t opam_package_name
      |> List.rev_map ~f:(fun (file, pkg) -> t, file, pkg))
    |> List.concat
    |> List.fold_left
         ~init:OpamPackage.Version.Map.empty
         ~f:(fun acc (repo, file, package) ->
           let version = OpamPackage.version package in
           if OpamPackage.Version.Map.mem version acc
           then acc
           else OpamPackage.Version.Map.add version (repo, file, package) acc)
    |> OpamPackage.Version.Map.values
    |> List.partition_map ~f:(fun (repo, file, pkg) ->
      match file with
      | Some file -> Left (repo, file, pkg)
      | None -> Right (repo, pkg))
  in
  let from_dirs =
    List.filter_map from_dirs ~f:(fun (repo, pkg) ->
      match repo.source with
      | Repo _ -> assert false
      | Directory dir ->
        load_opam_package_from_dir ~dir pkg
        |> Option.map ~f:(fun opam_file -> pkg, opam_file))
  in
  let+ from_git =
    match from_git with
    | [] -> Fiber.return []
    | packages ->
      let* rev_store = rev_store in
      let+ opam_files =
        List.map packages ~f:(fun (repo, file, _) -> repo, file)
        |> load_packages_from_git rev_store
      in
      List.map2 opam_files packages ~f:(fun opam_file (_, _, pkg) -> pkg, opam_file)
  in
  from_dirs @ from_git
  |> List.rev_map ~f:(fun (opam_package, opam_file) ->
    OpamPackage.version opam_package, opam_file)
  |> OpamPackage.Version.Map.of_list
;;

module Private = struct
  let create ~source ~repo_id =
    let packages_dir_path = Path.of_string "/" in
    let serializable =
      Option.map source ~f:(fun source -> { Serializable.repo_id; source })
    in
    { source = Directory packages_dir_path; serializable }
  ;;
end
