open Import
open Fiber.O

let collect local_packages =
  match
    Package_name.Map.values local_packages
    |> List.concat_map ~f:(fun (local_pacakge : Local_package.For_solver.t) ->
      Package_name.Map.to_list_map local_pacakge.pins ~f:(fun name (loc, version, url) ->
        let version = Package_version.to_opam_package_version version in
        name, (loc, version, url)))
    |> Package_name.Map.of_list
  with
  | Ok s -> s
  | Error (package, (loc, _, _), _) ->
    User_error.raise
      ~loc
      [ Pp.textf "local package %s cannot be pinned" (Package_name.to_string package) ]
;;

(* There's many layouts for pinned packages:
   - toplevel opam file
   - $name.opam file
   - opam/$name.opam
   - opam/opam

   This function tries them one by one. The [stat] argument is to there so that it works
   for both the local file system and the revision store. *)
let discover_layout loc ~stat name =
  let name_opam = Package_name.to_string name ^ ".opam" in
  let opam_file = Path.Local.of_string name_opam in
  let abort () =
    User_error.raise
      ~loc
      [ Pp.textf
          "unable to discover an opam file for package %s"
          (Package_name.to_string name)
      ]
  in
  let must_be_a_file p =
    User_error.raise
      ~loc
      [ Pp.textf "%s must be a file" (Path.Local.to_string_maybe_quoted p) ]
  in
  match stat opam_file with
  | `File -> opam_file, None
  | _ ->
    let opam_file_or_dir = Path.Local.of_string "opam" in
    (match stat opam_file_or_dir with
     | `File -> opam_file_or_dir, None
     | `Dir ->
       let opam_file = Path.Local.relative opam_file_or_dir name_opam in
       (match stat opam_file with
        | `File -> opam_file, None
        | `Dir -> must_be_a_file opam_file
        | `Absent_or_unrecognized ->
          let file = Path.Local.relative opam_file_or_dir "opam" in
          (match stat file with
           | `File -> opam_file, Some (Path.Local.relative opam_file_or_dir "files")
           | `Dir -> must_be_a_file file
           | `Absent_or_unrecognized -> abort ()))
     | `Absent_or_unrecognized -> abort ())
;;

let resolve_package loc name (url : OpamUrl.t) version =
  let package = OpamPackage.create (Package_name.to_opam_package_name name) version in
  let discover_layout = discover_layout loc name in
  let+ resolved_package =
    match OpamUrl.local_or_git_only url loc with
    | `Path dir ->
      let stat path =
        let path = Path.append_local dir path in
        match (Path.stat_exn path).st_kind with
        | S_REG -> `File
        | S_DIR -> `Dir
        | _ -> `Absent_or_unrecognized
        | exception Unix.Unix_error (ENOENT, _, _) -> `Absent_or_unrecognized
      in
      let opam_file_path, files_dir = discover_layout ~stat in
      Resolved_package.local_fs package ~dir ~opam_file_path ~files_dir |> Fiber.return
    | `Git ->
      let* rev = Opam_repo.Source.of_opam_url loc url >>= Opam_repo.Source.rev in
      let opam_file =
        match
          Rev_store.At_rev.directory_entries rev (Path.Local.of_string ".")
          |> Rev_store.File.Set.find ~f:(fun file ->
            Path.Local.equal (Rev_store.File.path file) (Path.Local.of_string "opam"))
        with
        | Some s -> s
        | None ->
          User_error.raise ~loc [ Pp.text "unable to find opam file in this repository" ]
      in
      let stat path =
        match
          Rev_store.File.Set.is_empty (Rev_store.At_rev.directory_entries rev path)
        with
        | false -> `Dir
        | true ->
          (match Path.Local.parent path with
           | None -> `Absent_or_unrecognized
           | Some parent ->
             let files = Rev_store.At_rev.directory_entries rev parent in
             let basename = Path.Local.basename path in
             if Rev_store.File.Set.exists files ~f:(fun file ->
                  let path = Rev_store.File.path file in
                  String.equal basename (Path.Local.basename path))
             then `File
             else `Absent_or_unrecognized)
      in
      let opam_file_path, files_dir = discover_layout ~stat in
      let+ opam_file_contents =
        (* CR-rgrinberg: not efficient to make such individual calls *)
        Rev_store.At_rev.content rev opam_file_path
        >>| function
        | Some p -> p
        | None ->
          Code_error.raise
            ~loc
            "unable to find file"
            [ "opam_file_path", Path.Local.to_dyn opam_file_path ]
      in
      Resolved_package.git_repo package ~opam_file ~opam_file_contents rev ~files_dir
  in
  Resolved_package.set_url resolved_package url
;;

let resolve pins =
  Package_name.Map.to_list pins
  |> Fiber.parallel_map ~f:(fun (name, (loc, version, url)) ->
    let+ resolved = resolve_package loc name url version in
    name, resolved)
  >>| Package_name.Map.of_list_exn
;;

let resolve_pins local_packages = collect local_packages |> resolve
