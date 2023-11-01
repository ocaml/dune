open Import
open Fiber.O

let ( / ) = Path.relative

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
    | Repo of Rev_store.Remote.At_rev.t

  let equal a b =
    match a, b with
    | Directory a, Directory b -> Path.equal a b
    | Repo a, Repo b -> Rev_store.Remote.At_rev.equal a b
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
  if not (Path.exists opam_repo_dir_path)
  then
    User_error.raise
      [ Pp.textf "%s does not exist" (Path.to_string_maybe_quoted opam_repo_dir_path) ];
  if not (Path.is_directory opam_repo_dir_path)
  then
    User_error.raise
      [ Pp.textf "%s is not a directory" (Path.to_string_maybe_quoted opam_repo_dir_path)
      ];
  let packages_dir_path = opam_repo_dir_path / "packages" in
  if not (Path.exists packages_dir_path && Path.is_directory packages_dir_path)
  then
    User_error.raise
      [ Pp.textf
          "%s doesn't look like a path to an opam repository as it lacks a subdirectory \
           named \"packages\""
          (Path.to_string_maybe_quoted opam_repo_dir_path)
      ];
  let serializable =
    Option.map source ~f:(fun source -> { Serializable.repo_id; source })
  in
  { source = Directory packages_dir_path; serializable }
;;

let xdg_repo_location =
  let ( / ) = Filename.concat in
  lazy (Xdg.cache_dir (Lazy.force Dune_util.xdg) / "dune/git-repo" |> Path.of_string)
;;

let of_git_repo ~repo_id ~source =
  let+ at_rev, computed_repo_id =
    let* remote =
      let* repo =
        let dir = Lazy.force xdg_repo_location in
        Rev_store.load_or_create ~dir
      in
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
      let repo_id = Option.map at_rev ~f:Rev_store.Remote.At_rev.repository_id in
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

(* Return the path to an "opam" file describing a particular package
   (name and version) from this opam repository. *)
let get_opam_file_path path opam_package =
  let name = opam_package |> OpamPackage.name |> OpamPackage.Name.to_string in
  path / name / OpamPackage.to_string opam_package / "opam" |> if_exists
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
  let name = opam_package |> OpamPackage.name |> OpamPackage.Name.to_string in
  match t.source with
  | Directory path ->
    (match path / name / OpamPackage.to_string opam_package / "files" |> if_exists with
     | None -> Fiber.return []
     | Some file_path ->
       let entries =
         scan_files_entries file_path
         |> List.map ~f:(fun local_file ->
           let original = File_entry.Path (Path.append_local file_path local_file) in
           { File_entry.local_file; original })
       in
       Fiber.return entries)
  | Repo at_rev ->
    let files_root =
      Path.Local.L.relative
        Path.Local.root
        [ "packages"; name; OpamPackage.to_string opam_package; "files" ]
    in
    Rev_store.Remote.At_rev.directory_entries at_rev files_root
    |> Path.Local.Set.to_list
    |> Fiber.parallel_map ~f:(fun remote_file ->
      Rev_store.Remote.At_rev.content at_rev remote_file
      >>| function
      | None ->
        Code_error.raise
          "Enumerated file in directory but file can't be retrieved"
          [ "local_file", Path.Local.to_dyn remote_file ]
      | Some content ->
        let local_file =
          Path.Local.descendant ~of_:files_root remote_file |> Option.value_exn
        in
        { File_entry.local_file; original = Content content })
;;

module With_file = struct
  type t =
    { opam_file : OpamFile.OPAM.t
    ; file : Path.t
    }
end

(* Reads an opam package definition from an "opam" file in this repository
   corresponding to a package (name and version). *)
let load_opam_package t opam_package =
  match t.source with
  | Directory d ->
    get_opam_file_path d opam_package
    |> Option.map ~f:(fun opam_file_path ->
      let opam_file =
        Path.to_string opam_file_path
        |> OpamFilename.raw
        |> OpamFile.make
        |> OpamFile.OPAM.read
      in
      { With_file.opam_file; file = opam_file_path })
    |> Fiber.return
  | Repo at_rev ->
    let expected_path =
      let package_name = opam_package |> OpamPackage.name |> OpamPackage.Name.to_string in
      let package_version =
        opam_package |> OpamPackage.version |> OpamPackage.Version.to_string
      in
      sprintf "packages/%s/%s.%s/opam" package_name package_name package_version
    in
    let file = Path.Local.of_string expected_path in
    Rev_store.Remote.At_rev.content at_rev file
    >>| Option.map ~f:(fun content ->
      let opam_file =
        (* the filename is used to read the version number *)
        let filename = OpamFile.make (OpamFilename.of_string expected_path) in
        OpamFile.OPAM.read_from_string ~filename content
      in
      (* TODO the [file] here is made up *)
      { With_file.opam_file; file = Path.source @@ Path.Source.of_local file })
;;

let get_opam_package_version_dir_path packages_dir_path opam_package_name =
  let p = packages_dir_path / OpamPackage.Name.to_string opam_package_name in
  if_exists p
;;

let all_package_versions t opam_package_name =
  match t.source with
  | Directory d ->
    (match get_opam_package_version_dir_path d opam_package_name with
     | None -> []
     | Some version_dir_path ->
       (match Path.readdir_unsorted version_dir_path with
        | Error e ->
          User_error.raise
            [ Pp.textf
                "Unable to read package versions from %s: %s"
                (Path.to_string_maybe_quoted version_dir_path)
                (Dune_filesystem_stubs.Unix_error.Detailed.to_string_hum e)
            ]
        | Ok version_dirs -> List.map version_dirs ~f:OpamPackage.of_string))
  | Repo at_rev ->
    let version_dir_path =
      let name = OpamPackage.Name.to_string opam_package_name in
      Path.Local.relative (Path.Local.of_string "packages") name
    in
    Rev_store.Remote.At_rev.directory_entries at_rev version_dir_path
    |> Path.Local.Set.to_list
    |> List.filter_map ~f:(fun dir_entry ->
      let open Option.O in
      Path.Local.basename_opt dir_entry
      >>= function
      | "opam" ->
        let+ parent = Path.Local.parent dir_entry in
        parent |> Path.Local.basename |> OpamPackage.of_string
      | _ -> None)
;;

let load_all_versions ts opam_package_name =
  List.map ts ~f:(fun t ->
    all_package_versions t opam_package_name |> List.rev_map ~f:(fun pkg -> t, pkg))
  |> List.concat
  |> List.fold_left ~init:OpamPackage.Version.Map.empty ~f:(fun acc (repo, package) ->
    let version = OpamPackage.version package in
    if OpamPackage.Version.Map.mem version acc
    then acc
    else OpamPackage.Version.Map.add version (repo, package) acc)
  |> OpamPackage.Version.Map.to_seq
  |> List.of_seq
  |> Fiber.parallel_map ~f:(fun (version, (repo, pkg)) ->
    load_opam_package repo pkg
    >>| Option.map ~f:(fun (pkg : With_file.t) -> version, (repo, pkg)))
  >>| List.filter_opt
  >>| OpamPackage.Version.Map.of_list
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
