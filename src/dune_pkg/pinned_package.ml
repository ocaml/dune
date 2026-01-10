open Import
open Fiber.O

let merge_pins name (pin1 : Local_package.pin) (pin2 : Local_package.pin) =
  let loc1, url1 = pin1.url in
  let loc2, url2 = pin2.url in
  if Package_version.equal pin1.version pin2.version
  then
    if OpamUrl.equal url1 url2
    then pin1
    else
      User_error.raise
        ~loc:loc1
        [ Pp.textf
            "local package %s is pinned twice with different URLs"
            (Package_name.to_string name)
        ; Pp.textf "it is also defined in %s" (Loc.to_file_colon_line loc2)
        ]
  else
    User_error.raise
      ~loc:loc1
      [ Pp.textf
          "local package %s is pinned here with version %s, but also version %s in %s"
          (Package_name.to_string name)
          (Package_version.to_string pin1.version)
          (Package_version.to_string pin2.version)
          (Loc.to_file_colon_line loc2)
      ]
;;

let collect (local_packages : Local_package.t Package_name.Map.t) =
  Package_name.Map.values local_packages
  |> List.concat_map ~f:(fun (local_package : Local_package.t) ->
    Package_name.Map.to_list_map local_package.pins ~f:(fun name pin -> name, pin))
  |> List.sort ~compare:(fun (_, (pin1 : Local_package.pin)) (_, pin2) ->
    Loc.compare pin1.loc pin2.loc)
  |> Package_name.Map.of_list_reducei ~f:merge_pins
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
  let* mount = Mount.of_opam_url loc_url url in
  let* opam_file_path, files_dir = discover_layout loc name mount in
  match Mount.backend mount with
  | Path dir ->
    let opam_file =
      let path = Path.append_local dir opam_file_path in
      let loc = Loc.in_file path in
      loc, Opam_file.opam_file_of_path path
    in
    Resolved_package.local_fs package opam_file ~dir ~files_dir ~url:(Some url)
    |> Fiber.return
  | Git rev ->
    let+ contents =
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
    let opam_file =
      let path = Path.of_local opam_file_path in
      let loc = Loc.in_file path in
      loc, Opam_file.opam_file_of_string_exn ~contents path
    in
    Resolved_package.git_repo
      package
      opam_file
      rev
      ~dune_build:false
      ~files_dir
      ~url:(Some url)
;;
