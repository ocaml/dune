open! Import

type t =
  { local_packages : Local_package.t Package_name.Map.t
  ; lock_dir : Lock_dir.t
  }

let lockdir_regenerate_hints =
  [ Pp.concat
      ~sep:Pp.space
      [ Pp.text
          "The lockdir no longer contains a solution for the local packages in this \
           project. Regenerate the lockdir by running:"
      ; User_message.command "dune pkg lock"
      ]
  ]
;;

let version_by_package_name t =
  let from_local_packages =
    Package_name.Map.map t.local_packages ~f:(fun local_package ->
      Option.value local_package.version ~default:Lock_dir.Pkg_info.default_version)
  in
  let from_lock_dir =
    Package_name.Map.map t.lock_dir.packages ~f:(fun pkg -> pkg.info.version)
  in
  let exception Duplicate_package of Package_name.t in
  try
    Ok
      (Package_name.Map.union
         from_local_packages
         from_lock_dir
         ~f:(fun duplicate_package_name _ _ ->
           raise (Duplicate_package duplicate_package_name)))
  with
  | Duplicate_package duplicate_package_name ->
    let local_package =
      Package_name.Map.find_exn t.local_packages duplicate_package_name
    in
    Error
      (User_message.make
         ~hints:lockdir_regenerate_hints
         ~loc:local_package.loc
         [ Pp.textf
             "A package named %S is defined locally but is also present in the lockdir"
             (Package_name.to_string local_package.name)
         ])
;;

let all_non_local_dependencies_of_local_packages t version_by_package_name =
  let open Result.O in
  let solver_env =
    Solver_stats.Expanded_variable_bindings.to_solver_env
      t.lock_dir.expanded_solver_variable_bindings
  in
  let+ all_dependencies_of_local_packages =
    Package_name.Map.values t.local_packages
    |> List.map ~f:(fun (local_package : Local_package.t) ->
      Local_package.(
        for_solver local_package |> For_solver.opam_filtered_dependency_formula)
      |> Resolve_opam_formula.filtered_formula_to_package_names
           ~stats_updater:None
           ~with_test:true
           solver_env
           version_by_package_name
      |> Result.map_error ~f:(function
        | `Formula_could_not_be_satisfied unsatisfied_formula_hints ->
        User_message.make
          ~hints:lockdir_regenerate_hints
          ~loc:local_package.loc
          (Pp.textf
             "The dependencies of local package %S could not be satisfied from the \
              lockdir:"
             (Package_name.to_string local_package.name)
           :: List.map
                unsatisfied_formula_hints
                ~f:Resolve_opam_formula.Unsatisfied_formula_hint.pp))
      |> Result.map ~f:Package_name.Set.of_list)
    |> Result.List.all
    |> Result.map ~f:Package_name.Set.union_all
  in
  Package_name.Set.diff
    all_dependencies_of_local_packages
    (Package_name.Set.of_keys t.local_packages)
;;

let check_for_unnecessary_packges_in_lock_dir
  t
  all_non_local_dependencies_of_local_packages
  =
  let locked_transitive_closure_of_local_package_dependencies =
    match
      Lock_dir.transitive_dependency_closure
        t.lock_dir
        all_non_local_dependencies_of_local_packages
    with
    | Ok x -> x
    | Error (`Missing_packages missing_packages) ->
      (* Resolving the dependency formulae would have failed if there were any missing packages in the lockdir. *)
      Code_error.raise
        "Missing packages from lockdir after confirming no missing packages in lockdir"
        [ "missing package", Package_name.Set.to_dyn missing_packages ]
  in
  let all_locked_packages = Package_name.Set.of_keys t.lock_dir.packages in
  let unneeded_packages_in_lock_dir =
    Package_name.Set.diff
      all_locked_packages
      locked_transitive_closure_of_local_package_dependencies
  in
  if Package_name.Set.is_empty unneeded_packages_in_lock_dir
  then Ok ()
  else (
    let packages =
      Package_name.Set.to_list unneeded_packages_in_lock_dir
      |> List.map ~f:(Package_name.Map.find_exn t.lock_dir.packages)
    in
    Error
      (User_message.make
         ~hints:lockdir_regenerate_hints
         [ Pp.text
             "The lockdir contains packages which are not among the transitive \
              dependencies of any local package:"
         ; Pp.enumerate packages ~f:(fun (package : Lock_dir.Pkg.t) ->
             Pp.textf
               "%s.%s"
               (Package_name.to_string package.info.name)
               (Package_version.to_string package.info.version))
         ]))
;;

let validate_dependency_hash { local_packages; lock_dir } =
  let local_packages =
    Package_name.Map.values local_packages |> List.map ~f:Local_package.for_solver
  in
  let regenerate_lock_dir_hints =
    [ Pp.concat
        ~sep:Pp.space
        [ Pp.text "Regenerate the lockdir by running"
        ; User_message.command "dune pkg lock"
        ]
    ]
  in
  let non_local_dependencies =
    Local_package.For_solver.list_non_local_dependency_set local_packages
  in
  let dependency_hash = Local_package.Dependency_set.hash non_local_dependencies in
  match lock_dir.dependency_hash, dependency_hash with
  | None, None -> Ok ()
  | Some (loc, lock_dir_dependency_hash), None ->
    Error
      (User_error.make
         ~loc
         ~hints:regenerate_lock_dir_hints
         [ Pp.textf
             "This project has no non-local dependencies yet the lockfile contains a \
              dependency hash: %s"
             (Local_package.Dependency_hash.to_string lock_dir_dependency_hash)
         ])
  | None, Some _ ->
    let any_non_local_dependency : Package_dependency.t =
      List.hd (Local_package.Dependency_set.package_dependencies non_local_dependencies)
    in
    Error
      (User_error.make
         ~hints:regenerate_lock_dir_hints
         [ Pp.text
             "This project has at least one non-local dependency but the lockdir doesn't \
              contain a dependency hash."
         ; Pp.textf
             "An example of a non-local dependency of this project is: %s"
             (Package_name.to_string any_non_local_dependency.name)
         ])
  | Some (loc, lock_dir_dependency_hash), Some non_local_dependency_hash ->
    if Local_package.Dependency_hash.equal
         lock_dir_dependency_hash
         non_local_dependency_hash
    then Ok ()
    else
      Error
        (User_error.make
           ~loc
           ~hints:regenerate_lock_dir_hints
           [ Pp.text
               "Dependency hash in lockdir does not match the hash of non-local \
                dependencies of this project. The lockdir expects the the non-local \
                dependencies to hash to:"
           ; Pp.text (Local_package.Dependency_hash.to_string lock_dir_dependency_hash)
           ; Pp.text "...but the non-local dependencies of this project hash to:"
           ; Pp.text (Local_package.Dependency_hash.to_string non_local_dependency_hash)
           ])
;;

let validate t =
  let open Result.O in
  let* () = validate_dependency_hash t in
  version_by_package_name t
  >>= all_non_local_dependencies_of_local_packages t
  >>= check_for_unnecessary_packges_in_lock_dir t
;;

let create local_packages lock_dir =
  let open Result.O in
  let t = { local_packages; lock_dir } in
  let+ () = validate t in
  t
;;
