open Import
open Fiber.O

module Paths = struct
  let packages = Path.Local.of_string "packages"
end

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

(* Module for keeping track of the location of each opam file in an opam
   repository. Note that the opam spec allows for arbitrary directory
   hierarchies. *)
module Opam_file_table = struct
  module Entry = struct
    type t =
      { opam_file_path_relative_to_repo_root : Path.Local.t
      ; rev_store_file_if_loaded_from_git_repo : Rev_store.File.t option
        (* If this data was sourced from a git repo, this field retains a
           reference to the representation of this opam file in the repo because
           it's expensive to look up the file from its path when it's needed
           later. *)
      }

    let to_dyn t =
      Dyn.record
        [ ( "opam_file_path_relative_to_repo_root"
          , Path.Local.to_dyn t.opam_file_path_relative_to_repo_root )
        ; ( "rev_store_file_if_loaded_from_git_repo"
          , Dyn.option Rev_store.File.to_dyn t.rev_store_file_if_loaded_from_git_repo )
        ]
    ;;

    let equal
          t
          { opam_file_path_relative_to_repo_root; rev_store_file_if_loaded_from_git_repo }
      =
      Path.Local.equal
        t.opam_file_path_relative_to_repo_root
        opam_file_path_relative_to_repo_root
      && Option.equal
           Rev_store.File.equal
           t.rev_store_file_if_loaded_from_git_repo
           rev_store_file_if_loaded_from_git_repo
    ;;
  end

  module Version_to_entry = struct
    type t = Entry.t Package_version.Map.t

    let to_dyn = Package_version.Map.to_dyn Entry.to_dyn
    let equal = Package_version.Map.equal ~equal:Entry.equal
  end

  type t = Version_to_entry.t Package_name.Map.t

  let empty = Package_name.Map.empty
  let to_dyn = Package_name.Map.to_dyn Version_to_entry.to_dyn
  let equal = Package_name.Map.equal ~equal:Version_to_entry.equal

  let opam_file_path_of_package (t : t) package =
    let name = Package_name.of_opam_package_name (OpamPackage.name package) in
    Option.bind (Package_name.Map.find t name) ~f:(fun version_to_entry ->
      let version =
        Package_version.of_opam_package_version (OpamPackage.version package)
      in
      Option.map
        (Package_version.Map.find version_to_entry version)
        ~f:(fun (entry : Entry.t) -> entry.opam_file_path_relative_to_repo_root))
  ;;

  let files_dir_path_of_package (t : t) package =
    Option.map (opam_file_path_of_package t package) ~f:(fun opam_file_path ->
      Path.Local.relative (Path.Local.parent_exn opam_file_path) "files")
  ;;

  let all_package_versions_of_name (t : t) package_name =
    match Package_name.Map.find t package_name with
    | None -> []
    | Some version_to_entry ->
      Package_version.Map.keys version_to_entry
      |> List.map ~f:(fun package_version ->
        OpamPackage.create
          (Package_name.to_opam_package_name package_name)
          (Package_version.to_opam_package_version package_version))
  ;;

  let add t name version entry ~repo_name_for_messages ~loc =
    Package_name.Map.update t name ~f:(function
      | None -> Some (Package_version.Map.singleton version entry)
      | Some version_to_entry ->
        Some
          (Package_version.Map.update version_to_entry version ~f:(function
             | None -> Some entry
             | Some existing_entry ->
               User_warning.emit
                 ~loc
                 [ Pp.textf
                     "Multiple occurrences of package %s.%s in repo %S:"
                     (Package_name.to_string name)
                     (Package_version.to_string version)
                     repo_name_for_messages
                 ; Pp.textf
                     " - %s"
                     (Path.Local.to_string
                        existing_entry.Entry.opam_file_path_relative_to_repo_root)
                 ; Pp.textf
                     " - %s"
                     (Path.Local.to_string entry.opam_file_path_relative_to_repo_root)
                 ; Pp.newline
                 ; Pp.textf
                     "Dune will use the metadata in %s"
                     (Path.Local.to_string
                        existing_entry.opam_file_path_relative_to_repo_root)
                 ];
               Some existing_entry)))
  ;;

  (* Traverse a directory hierarchy looking for files named "opam". *)
  let of_repo_dir_path opam_repo_dir_path ~loc : t =
    let repo_name_for_messages = Path.to_absolute_filename opam_repo_dir_path in
    try
      Fpath.traverse_files
        ~dir:
          (Path.to_absolute_filename
             (Path.append_local opam_repo_dir_path Paths.packages))
        ~init:empty
        ~f:(fun ~dir file acc ->
          if String.equal file "opam"
          then (
            let opam_file_path_relative_to_repo_root =
              Path.Local.relative (Path.Local.relative Paths.packages dir) file
            in
            match OpamPackage.of_string_opt (Filename.basename dir) with
            | None ->
              User_warning.emit
                ~loc
                [ Pp.textf
                    "The repo %S contains an opam file at location %S which cannot be \
                     interpreted as an opam package. Opam files must be contained in \
                     directories named like <name>.<version>."
                    repo_name_for_messages
                    (Path.Local.relative Paths.packages dir |> Path.Local.to_string)
                ];
              acc
            | Some package ->
              let name = Package_name.of_opam_package_name (OpamPackage.name package) in
              let version =
                Package_version.of_opam_package_version (OpamPackage.version package)
              in
              let entry =
                { Entry.opam_file_path_relative_to_repo_root
                ; rev_store_file_if_loaded_from_git_repo = None
                }
              in
              add acc name version entry ~repo_name_for_messages ~loc)
          else acc)
    with
    | Unix.Unix_error (err, a, e) ->
      User_error.raise
        ~loc
        [ Pp.textf
            "Unable to scan the contents of the opam repo %S:"
            repo_name_for_messages
        ; Unix_error.Detailed.pp (err, a, e)
        ]
  ;;

  (* Traverse the set of files in the given rev store revision looking for
     files named "opam". *)
  let of_rev_store_at_rev at_rev ~repo_name_for_messages ~loc : t =
    Rev_store.At_rev.directory_entries ~recursive:true at_rev Paths.packages
    |> Rev_store.File.Set.fold ~init:empty ~f:(fun file acc ->
      let path = Rev_store.File.path file in
      if String.equal (Path.Local.basename path) "opam"
      then (
        let dir = Path.Local.parent_exn path in
        match OpamPackage.of_string_opt (Path.Local.basename dir) with
        | None ->
          User_warning.emit
            [ Pp.textf
                "The repo %S contains an opam file at location %S which cannot be \
                 interpreted as an opam package. Opam files must be contained in \
                 directories named like <name>.<version>."
                repo_name_for_messages
                (Path.Local.to_string dir)
            ];
          acc
        | Some package ->
          let name = Package_name.of_opam_package_name (OpamPackage.name package) in
          let version =
            Package_version.of_opam_package_version (OpamPackage.version package)
          in
          let entry =
            { Entry.opam_file_path_relative_to_repo_root = path
            ; rev_store_file_if_loaded_from_git_repo = Some file
            }
          in
          add acc name version entry ~repo_name_for_messages ~loc)
      else acc)
  ;;
end

type t =
  { source : Source_backend.t
  ; loc : Loc.t
  ; serializable : Serializable.t option
  ; opam_file_table : Opam_file_table.t
  }

let to_dyn { source; loc; serializable; opam_file_table } =
  Dyn.record
    [ "source", Source_backend.to_dyn source
    ; "loc", Loc.to_dyn loc
    ; "serializable", Dyn.option Serializable.to_dyn serializable
    ; "opam_table", Opam_file_table.to_dyn opam_file_table
    ]
;;

let equal { source; serializable; loc; opam_file_table } t =
  Source_backend.equal source t.source
  && Option.equal Serializable.equal serializable t.serializable
  && Loc.equal loc t.loc
  && Opam_file_table.equal opam_file_table t.opam_file_table
;;

let serializable { serializable; _ } = serializable

let of_opam_repo_dir_path loc opam_repo_dir_path =
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
  let opam_file_table = Opam_file_table.of_repo_dir_path opam_repo_dir_path ~loc in
  { source = Directory opam_repo_dir_path; serializable = None; loc; opam_file_table }
;;

let of_git_repo loc url =
  let+ at_rev =
    let* rev_store = Rev_store.get in
    OpamUrl.resolve url ~loc rev_store
    >>= (function
     | Error _ as e -> Fiber.return e
     | Ok s -> OpamUrl.fetch_revision url ~loc s rev_store)
    >>| User_error.ok_exn
  in
  let serializable =
    Some
      (sprintf
         "%s#%s"
         (OpamUrl.base_url url)
         (Rev_store.Object.to_string (Rev_store.At_rev.rev at_rev))
       |> OpamUrl.of_string
       |> OpamUrl.to_string)
  in
  let opam_file_table =
    Opam_file_table.of_rev_store_at_rev
      at_rev
      ~repo_name_for_messages:(OpamUrl.to_string url)
      ~loc
  in
  { source = Repo at_rev; serializable; loc; opam_file_table }
;;

let revision t =
  match t.source with
  | Repo r -> r
  | Directory _ -> Code_error.raise "not a git repo" []
;;

let load_opam_package_from_dir ~(dir : Path.t) opam_file_table package =
  Opam_file_table.opam_file_path_of_package opam_file_table package
  |> Option.bind ~f:(fun opam_file_path ->
    match Path.exists (Path.append_local dir opam_file_path) with
    | false -> None
    | true ->
      let files_dir = Opam_file_table.files_dir_path_of_package opam_file_table package in
      Some (Resolved_package.local_fs package ~dir ~opam_file_path ~files_dir))
;;

let load_packages_from_git rev_store opam_packages =
  let+ contents =
    List.map opam_packages ~f:(fun (file, _, _, _) -> file)
    |> Rev_store.content_of_files rev_store
  in
  List.map2
    opam_packages
    contents
    ~f:(fun (opam_file, package, rev, files_dir) opam_file_contents ->
      Resolved_package.git_repo
        package
        ~opam_file:(Rev_store.File.path opam_file)
        ~opam_file_contents
        rev
        ~files_dir:(Some files_dir))
;;

let all_packages_versions_at_rev opam_file_table opam_package_name =
  match
    Package_name.Map.find
      opam_file_table
      (Package_name.of_opam_package_name opam_package_name)
  with
  | None -> []
  | Some version_to_entry ->
    Package_version.Map.to_list_map
      version_to_entry
      ~f:(fun package_version (entry : Opam_file_table.Entry.t) ->
        Option.map entry.rev_store_file_if_loaded_from_git_repo ~f:(fun file ->
          let package =
            OpamPackage.create
              opam_package_name
              (Package_version.to_opam_package_version package_version)
          in
          file, package))
    |> List.filter_opt
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
  | Directory _ ->
    Opam_file_table.all_package_versions_of_name
      t.opam_file_table
      (Package_name.of_opam_package_name opam_package_name)
    |> List.map ~f:(fun pkg -> Key.Directory pkg)
  | Repo rev ->
    all_packages_versions_at_rev t.opam_file_table opam_package_name
    |> List.filter_map ~f:(fun (file, pkg) ->
      Opam_file_table.files_dir_path_of_package t.opam_file_table pkg
      |> Option.map ~f:(fun files_dir -> Key.Git (file, pkg, rev, files_dir)))
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
      | Git (file, pkg, rev, files_dir) -> Left (file, pkg, rev, files_dir)
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
        load_opam_package_from_dir ~dir repo.opam_file_table pkg
        |> Option.map ~f:(fun resolved_package -> pkg, resolved_package))
  in
  let+ from_git =
    match from_git with
    | [] -> Fiber.return []
    | packages ->
      let* rev_store = Rev_store.get in
      let+ resolved_packages = load_packages_from_git rev_store packages in
      List.map2 resolved_packages packages ~f:(fun resolved_package (_, pkg, _, _) ->
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

module Private = struct
  let create ~source:serializable =
    let packages_dir_path = Path.of_string "/" in
    { source = Directory packages_dir_path
    ; serializable
    ; loc = Loc.none
    ; opam_file_table = Opam_file_table.empty
    }
  ;;
end
