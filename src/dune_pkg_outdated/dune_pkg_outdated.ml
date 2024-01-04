open Import

type candidate =
  { is_immediate_dep_of_local_package : bool
  ; name : Package_name.t
  ; outdated_version : Package_version.t
  ; newer_version : Package_version.t
  }

type result =
  | Better_candidate of candidate
  | Package_not_found of Package_name.t
  | Package_is_best_candidate

type t = result list

let total_number_of_packages l = List.length l

let outdated_packages l =
  List.filter_map l ~f:(function
    | Better_candidate entry -> Some entry
    | _ -> None)
;;

let number_of_outdated_packages l = outdated_packages l |> List.length

let number_of_outdated_packages_that_are_immediate_deps l =
  outdated_packages l
  |> List.filter ~f:(fun x -> x.is_immediate_dep_of_local_package)
  |> List.length
;;

let packages_that_were_not_found l =
  List.filter_map l ~f:(function
    | Package_not_found name -> Some name
    | _ -> None)
;;

let explain_results_to_user results ~transitive ~lock_dir_path =
  let number_of_outdated_immediate_deps =
    number_of_outdated_packages_that_are_immediate_deps results
  in
  let number_of_outdated_deps = number_of_outdated_packages results in
  let total_number_of_deps = total_number_of_packages results in
  (* Depending on the number of immediate outdated and transitive outdated dependencies
     we give different messages. Therefore we need to determine what we have. *)
  let transitive_status =
    if number_of_outdated_deps = 0
    then `No_transitive_deps_outdated
    else if number_of_outdated_deps = total_number_of_deps
    then `All_transitive_deps_outdated
    else `Some_transitive_deps_outdated
  in
  let transitive_helper ~all_of =
    if transitive || number_of_outdated_immediate_deps = number_of_outdated_deps
    then []
    else
      [ Pp.text
          ("Showing immediate dependencies, use --transitive to see "
           ^ if all_of then "them all." else "the rest.")
      ]
  in
  let packages_in_lockdir_are ~all_of count =
    (Pp.tag User_message.Style.Warning
     @@ Pp.textf
          "%d/%d packages in %s are outdated."
          count
          total_number_of_deps
          (Path.Source.to_string_maybe_quoted lock_dir_path))
    :: transitive_helper ~all_of
  in
  match transitive_status with
  (* If there are no outdated transitive deps then everything is up to date. *)
  | `No_transitive_deps_outdated ->
    [ Pp.tag User_message.Style.Success
      @@ Pp.textf "%s is up to date." (Path.Source.to_string_maybe_quoted lock_dir_path)
    ]
  | `All_transitive_deps_outdated ->
    packages_in_lockdir_are ~all_of:true number_of_outdated_deps
  | `Some_transitive_deps_outdated ->
    packages_in_lockdir_are ~all_of:false number_of_outdated_deps
;;

let better_candidate
  ~repos
  ~(local_packages : Dune_pkg.Local_package.t Package_name.Map.t)
  (pkg : Lock_dir.Pkg.t)
  =
  let open Fiber.O in
  let pkg_name = pkg.info.name |> Package_name.to_string |> OpamPackage.Name.of_string in
  let is_immediate_dep_of_local_package =
    Package_name.Map.exists local_packages ~f:(fun local_package ->
      Dune_pkg.Local_package.(
        for_solver local_package |> For_solver.opam_filtered_dependency_formula)
      |> OpamFilter.filter_deps
           ~build:true
           ~post:false
           ~dev:false
           ~default:false
           ~test:false
           ~doc:false
      |> OpamFormula.atoms
      |> List.exists ~f:(fun (name', _) -> OpamPackage.Name.equal pkg_name name'))
  in
  let+ all_versions =
    Opam_repo.load_all_versions repos pkg_name
    >>| OpamPackage.Version.Map.values
    >>| List.map ~f:Resolved_package.opam_file
  in
  match
    List.max all_versions ~f:(fun x y ->
      Ordering.of_int
        (OpamPackage.Version.compare (OpamFile.OPAM.version x) (OpamFile.OPAM.version y)))
  with
  | None -> Package_not_found pkg.info.name
  | Some newest_opam_file ->
    let version = OpamFile.OPAM.version newest_opam_file in
    (match
       Package_version.to_opam_package_version pkg.info.version
       |> OpamPackage.Version.compare version
       |> Ordering.of_int
     with
     | Lt | Eq -> Package_is_best_candidate
     | Gt ->
       Better_candidate
         { is_immediate_dep_of_local_package
         ; name = pkg.info.name
         ; newer_version = version |> Package_version.of_opam_package_version
         ; outdated_version = pkg.info.version
         })
;;

let pp results ~transitive ~lock_dir_path =
  let outdated_packages =
    match
      List.filter_map
        (outdated_packages results)
        ~f:
          (fun
            { is_immediate_dep_of_local_package; name; outdated_version; newer_version }
          ->
          (* If --transitive is passed, then we always print the available package. If
             not, then we only print it if it is an immediate dependency of a local
             package. *)
          if transitive || is_immediate_dep_of_local_package
          then
            (* CR-someday alizter: Create table printing helpers in Console and use
               those to align output. *)
            Some
              (Pp.concat
                 [ Pp.verbatim (Dune_lang.Package_name.to_string name)
                 ; Pp.space
                 ; Pp.tag
                     (User_message.Style.Ansi_styles [ `Fg_bright_red ])
                     (Pp.verbatim (Package_version.to_string outdated_version))
                 ; Pp.text " < "
                 ; Pp.tag
                     (User_message.Style.Ansi_styles [ `Fg_bright_green ])
                     (Pp.verbatim (Package_version.to_string newer_version))
                 ])
          else None)
    with
    | [] -> []
    | outdated_packages -> [ Pp.enumerate ~f:Fun.id outdated_packages ]
  in
  explain_results_to_user ~transitive ~lock_dir_path results @ outdated_packages
  |> Pp.concat_map ~sep:Pp.space ~f:Pp.box
  |> Pp.vbox
;;

let find ~repos ~local_packages packages =
  Package_name.Map.to_list_map packages ~f:(fun _name pkg ->
    better_candidate ~repos ~local_packages pkg)
  |> Fiber.all_concurrently
;;

module For_tests = struct
  type nonrec result = result

  let package_is_best_candidate = Package_is_best_candidate

  let better_candidate
    ~is_immediate_dep_of_local_package
    ~name
    ~newer_version
    ~outdated_version
    =
    Better_candidate
      { is_immediate_dep_of_local_package
      ; name = Package_name.of_string name
      ; newer_version
      ; outdated_version
      }
  ;;

  let explain_results = explain_results_to_user
  let pp = pp
end
