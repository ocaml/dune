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

let of_git_repo ~repo_id ~update ~source =
  let+ at_rev, computed_repo_id =
    let* remote =
      let* repo = rev_store in
      let* remote = Rev_store.add_repo repo ~source in
      match update with
      | true -> Rev_store.Remote.update remote
      | false -> Fiber.return @@ Rev_store.Remote.don't_update remote
    in
    match repo_id with
    | Some repo_id ->
      let+ at_rev = Rev_store.Remote.rev_of_repository_id remote repo_id in
      at_rev, Some repo_id
    | None ->
      let+ at_rev =
        let name = Rev_store.Remote.default_branch remote in
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

module With_file = struct
  type extra_files =
    | Inside_files_dir
    | Git_files of Rev_store.File.t list

  type nonrec t =
    { opam_file : OpamFile.OPAM.t
    ; package : OpamPackage.t
    ; opam_file_path : Path.Local.t
    ; repo : t
    ; extra_files : extra_files
    }

  let file t =
    match t.repo.source with
    | Directory d -> Path.append_local d t.opam_file_path
    | Repo _ ->
      (* XXX fake path *)
      Path.source @@ Path.Source.of_local t.opam_file_path
  ;;

  let package t = t.package
  let opam_file t = t.opam_file
  let repo t = t.repo
end

let load_opam_package_from_dir ~(dir : Path.t) package =
  let opam_file_path = Paths.opam_file package in
  Path.append_local dir opam_file_path
  |> if_exists
  |> Option.map ~f:(fun opam_file_path_in_repo ->
    let opam_file =
      Path.to_string opam_file_path_in_repo
      |> OpamFilename.raw
      |> OpamFile.make
      |> OpamFile.OPAM.read
    in
    { With_file.opam_file
    ; package
    ; opam_file_path
    ; repo = { source = Backend.Directory dir; serializable = None }
    ; extra_files = With_file.Inside_files_dir
    })
;;

let get_opam_package_files with_files =
  let indexed = List.mapi with_files ~f:(fun i w -> i, w) |> Int.Map.of_list_exn in
  let from_dirs, from_git =
    Int.Map.partition_map indexed ~f:(fun (with_file : With_file.t) ->
      match with_file.extra_files with
      | Git_files files -> Right (with_file.package, files)
      | Inside_files_dir ->
        let dir =
          match with_file.repo.source with
          | Directory root -> Path.append_local root (Paths.files_dir with_file.package)
          | Repo _ -> assert false
        in
        Left dir)
  in
  let+ from_git =
    if Int.Map.is_empty from_git
    then Fiber.return []
    else (
      let files_with_idx =
        Int.Map.to_list from_git
        |> List.concat_map ~f:(fun (idx, (package, files)) ->
          List.map files ~f:(fun file -> idx, package, file))
      in
      let* rev_store = rev_store in
      List.map files_with_idx ~f:(fun (_, _, file) -> file)
      |> Rev_store.content_of_files rev_store
      >>| List.map2 files_with_idx ~f:(fun (idx, package, file) content ->
        let entry =
          let local_file =
            Path.Local.descendant
              ~of_:(Paths.files_dir package)
              (Rev_store.File.path file)
            |> Option.value_exn
          in
          { File_entry.local_file; original = Content content }
        in
        idx, entry))
  in
  let from_dirs =
    Int.Map.to_list from_dirs
    |> List.concat_map ~f:(fun (idx, files_dir) ->
      scan_files_entries files_dir
      |> List.map ~f:(fun local_file ->
        ( idx
        , { File_entry.original = Path (Path.append_local files_dir local_file)
          ; local_file
          } )))
  in
  List.rev_append from_git from_dirs
  |> Int.Map.of_list_multi
  |> Int.Map.merge indexed ~f:(fun _ pkg files ->
    match pkg with
    | None -> assert false
    | Some _ -> Some (Option.value files ~default:[]))
  |> Int.Map.values
;;

let load_packages_from_git rev_store opam_packages =
  let+ contents =
    List.map opam_packages ~f:(fun (_, file, _, _) -> file)
    |> Rev_store.content_of_files rev_store
  in
  List.map2
    opam_packages
    contents
    ~f:(fun (repo, file, package, extra_files) opam_file_contents ->
      let opam_file_path = Rev_store.File.path file in
      let opam_file =
        let filename =
          (* the filename is used to read the version number *)
          Path.Local.to_string opam_file_path |> OpamFilename.of_string |> OpamFile.make
        in
        OpamFile.OPAM.read_from_string ~filename opam_file_contents
      in
      (* TODO the [file] here is made up *)
      { With_file.opam_file; opam_file_path; repo; extra_files; package })
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
    |> List.map ~f:(fun pkg -> `Directory pkg)
  | Repo rev ->
    all_packages_versions_at_rev rev opam_package_name
    |> List.map ~f:(fun (file, pkg) ->
      let extra_files : With_file.extra_files =
        let dir = Paths.files_dir pkg in
        Git_files
          (Rev_store.At_rev.directory_entries rev dir |> Rev_store.File.Set.to_list)
      in
      `Git (file, pkg, extra_files))
;;

let load_all_versions ts opam_package_name =
  let from_git, from_dirs =
    List.map ts ~f:(fun t ->
      all_package_versions t opam_package_name |> List.rev_map ~f:(fun pkg -> t, pkg))
    |> List.concat
    |> List.fold_left ~init:OpamPackage.Version.Map.empty ~f:(fun acc (repo, pkg) ->
      let version =
        let pkg =
          match pkg with
          | `Directory pkg -> pkg
          | `Git (_, pkg, _) -> pkg
        in
        OpamPackage.version pkg
      in
      if OpamPackage.Version.Map.mem version acc
      then acc
      else OpamPackage.Version.Map.add version (repo, pkg) acc)
    |> OpamPackage.Version.Map.values
    |> List.partition_map ~f:(fun (repo, pkg) ->
      match pkg with
      | `Git (file, pkg, extra_files) -> Left (repo, file, pkg, extra_files)
      | `Directory pkg -> Right (repo, pkg))
  in
  let from_dirs =
    List.filter_map from_dirs ~f:(fun (repo, pkg) ->
      match repo.source with
      | Repo _ ->
        Code_error.raise
          "impossible because all elements in from_dirs are from a directory"
          []
      | Directory dir ->
        load_opam_package_from_dir ~dir pkg
        |> Option.map ~f:(fun opam_file -> pkg, opam_file))
  in
  let+ from_git =
    match from_git with
    | [] -> Fiber.return []
    | packages ->
      let* rev_store = rev_store in
      let+ opam_files = load_packages_from_git rev_store packages in
      List.map2 opam_files packages ~f:(fun opam_file (_, _, pkg, _) -> pkg, opam_file)
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
