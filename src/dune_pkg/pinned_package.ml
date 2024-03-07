open Import
open Fiber.O

let collect (local_packages : Local_package.t Package_name.Map.t) =
  match
    Package_name.Map.values local_packages
    |> List.concat_map ~f:(fun (local_package : Local_package.t) ->
      Package_name.Map.to_list_map local_package.pins ~f:(fun name pin -> name, pin))
    |> Package_name.Map.of_list
  with
  | Ok s -> s
  | Error (_, pin, _) ->
    User_error.raise
      ~loc:pin.loc
      [ Pp.textf "local package %s cannot be pinned" (Package_name.to_string pin.name) ]
;;

(* There's many layouts for pinned packages:
   - toplevel opam file
   - $name.opam file
   - opam/$name.opam
   - opam/opam *)
let discover_layout loc name mount =
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
  Mount.stat mount opam_file
  >>= function
  | `File -> Fiber.return (opam_file, None)
  | _ ->
    let opam_file_or_dir = Path.Local.of_string "opam" in
    Mount.stat mount opam_file_or_dir
    >>= (function
     | `File -> Fiber.return (opam_file_or_dir, None)
     | `Absent_or_unrecognized -> abort ()
     | `Dir ->
       let opam_file = Path.Local.relative opam_file_or_dir name_opam in
       Mount.stat mount opam_file
       >>= (function
        | `File -> Fiber.return (opam_file, None)
        | `Dir -> must_be_a_file opam_file
        | `Absent_or_unrecognized ->
          let file = Path.Local.relative opam_file_or_dir "opam" in
          Mount.stat mount file
          >>| (function
           | `File -> file, Some (Path.Local.relative opam_file_or_dir "files")
           | `Dir -> must_be_a_file file
           | `Absent_or_unrecognized -> abort ())))
;;

let resolve_package { Local_package.loc; url = loc_url, url; name; version; origin = _ } =
  let package =
    OpamPackage.create
      (Package_name.to_opam_package_name name)
      (Package_version.to_opam_package_version version)
  in
  let+ resolved_package =
    let* mount = Mount.of_opam_url loc_url url in
    let* opam_file_path, files_dir = discover_layout loc name mount in
    match Mount.backend mount with
    | Path dir ->
      Resolved_package.local_fs package ~dir ~opam_file_path ~files_dir |> Fiber.return
    | Git rev ->
      let+ opam_file_contents =
        (* CR-rgrinberg: not efficient to make such individual calls *)
        Mount.read mount opam_file_path
        >>| function
        | Some p -> p
        | None ->
          let files =
            match Path.Local.parent opam_file_path with
            | None -> []
            | Some parent ->
              Rev_store.At_rev.directory_entries rev parent ~recursive:false
              |> Rev_store.File.Set.to_list_map ~f:Rev_store.File.path
          in
          Code_error.raise
            ~loc
            "unable to find file"
            [ "opam_file_path", Path.Local.to_dyn opam_file_path
            ; "files", Dyn.list Path.Local.to_dyn files
            ]
      in
      Resolved_package.git_repo
        package
        ~opam_file:opam_file_path
        ~opam_file_contents
        rev
        ~files_dir
  in
  Resolved_package.set_url resolved_package url
;;
