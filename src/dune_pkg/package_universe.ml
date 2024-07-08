open! Import

type t =
  { local_packages : Local_package.t Package_name.Map.t
  ; lock_dir : Lock_dir.t
  ; version_by_package_name : Package_version.t Package_name.Map.t
  ; solver_env : Solver_env.t
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

let version_by_package_name local_packages (lock_dir : Lock_dir.t) =
  let from_local_packages =
    Package_name.Map.map local_packages ~f:(fun (local_package : Local_package.t) ->
      Option.value local_package.version ~default:Lock_dir.Pkg_info.default_version)
  in
  let from_lock_dir =
    Package_name.Map.map lock_dir.packages ~f:(fun pkg -> pkg.info.version)
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
    let local_package = Package_name.Map.find_exn local_packages duplicate_package_name in
    Error
      (User_message.make
         ~hints:lockdir_regenerate_hints
         ~loc:local_package.loc
         [ Pp.textf
             "A package named %S is defined locally but is also present in the lockdir"
             (Package_name.to_string local_package.name)
         ])
;;

let concrete_dependencies_of_local_package t local_package_name ~with_test =
  let local_package = Package_name.Map.find_exn t.local_packages local_package_name in
  Local_package.(for_solver local_package |> For_solver.opam_filtered_dependency_formula)
  |> Resolve_opam_formula.filtered_formula_to_package_names
       ~with_test
       (Solver_env.to_env t.solver_env)
       t.version_by_package_name
  |> Result.map_error ~f:(function
    | `Formula_could_not_be_satisfied unsatisfied_formula_hints ->
    User_message.make
      ?hints:(Option.some_if with_test lockdir_regenerate_hints)
      ~loc:local_package.loc
      (Pp.textf
         "The dependencies of local package %S could not be satisfied from the lockdir%s:"
         (Package_name.to_string local_package.name)
         (if with_test
          then ""
          else " when the solver variable 'with_test' is set to 'false'")
       :: List.map
            unsatisfied_formula_hints
            ~f:Resolve_opam_formula.Unsatisfied_formula_hint.pp))
;;

let all_non_local_dependencies_of_local_packages t =
  let open Result.O in
  let+ all_dependencies_of_local_packages =
    Package_name.Map.keys t.local_packages
    |> Result.List.map ~f:(fun p ->
      concrete_dependencies_of_local_package ~with_test:true t p
      |> Result.map ~f:Package_name.Set.of_list)
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
  let unneeded_packages_in_lock_dir =
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

let validate_dependency_hash { local_packages; lock_dir; _ } =
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
  all_non_local_dependencies_of_local_packages t
  >>= check_for_unnecessary_packges_in_lock_dir t
;;

let create local_packages lock_dir =
  let open Result.O in
  let* version_by_package_name = version_by_package_name local_packages lock_dir in
  let solver_env =
    Solver_stats.Expanded_variable_bindings.to_solver_env
      lock_dir.expanded_solver_variable_bindings
  in
  let t = { local_packages; lock_dir; version_by_package_name; solver_env } in
  let+ () = validate t in
  t
;;

let local_transitive_dependency_closure_without_test =
  let module Top_closure = Top_closure.Make (Package_name.Set) (Monad.Id) in
  fun t start ->
    match
      Top_closure.top_closure
        ~deps:(fun a ->
          concrete_dependencies_of_local_package t a ~with_test:false
          |> User_error.ok_exn
          |> List.filter ~f:(Package_name.Map.mem t.local_packages))
        ~key:Fun.id
        start
    with
    | Ok s -> Package_name.Set.of_list s
    | Error _ -> Code_error.raise "cycles aren't allowed because we forbid post deps" []
;;

let transitive_dependency_closure_without_test t start =
  let local_package_names = Package_name.Set.of_keys t.local_packages in
  let local_transitive_dependency_closure =
    local_transitive_dependency_closure_without_test
      t
      (Package_name.Set.inter local_package_names start |> Package_name.Set.to_list)
  in
  let non_local_transitive_dependency_closure =
    let non_local_immediate_dependencies_of_local_transitive_dependency_closure =
      local_transitive_dependency_closure
      |> Package_name.Set.to_list
      |> Package_name.Set.union_map ~f:(fun name ->
        let all_deps =
          concrete_dependencies_of_local_package t name ~with_test:false
          |> User_error.ok_exn
          |> Package_name.Set.of_list
        in
        Package_name.Set.diff all_deps local_package_names)
    in
    Lock_dir.transitive_dependency_closure
      t.lock_dir
      Package_name.Set.(
        union
          non_local_immediate_dependencies_of_local_transitive_dependency_closure
          (diff start local_package_names))
    |> function
    | Ok x -> x
    | Error (`Missing_packages missing_packages) ->
      Code_error.raise
        "Attempted to find non-existent packages in lockdir after validation which \
         should not be possible"
        (Package_name.Set.to_list missing_packages
         |> List.map ~f:(fun p -> "missing package", Package_name.to_dyn p))
  in
  Package_name.Set.union
    local_transitive_dependency_closure
    non_local_transitive_dependency_closure
;;

let contains_package t package_name =
  let in_local_packages = Package_name.Map.mem t.local_packages package_name in
  let in_lock_dir = Package_name.Map.mem t.lock_dir.packages package_name in
  in_local_packages || in_lock_dir
;;

let check_contains_package t package_name =
  if not (contains_package t package_name)
  then
    User_error.raise
      [ Pp.textf
          "Package %S is neither a local package nor present in the lockdir."
          (Package_name.to_string package_name)
      ]
;;

let all_dependencies t package ~traverse =
  check_contains_package t package;
  let immediate_deps =
    match concrete_dependencies_of_local_package t package ~with_test:true with
    | Ok x -> Package_name.Set.of_list x
    | Error e ->
      Code_error.raise
        "Invalid package universe which should have already been validated"
        [ "error", Dyn.string (User_message.to_string e) ]
  in
  match traverse with
  | `Immediate -> immediate_deps
  | `Transitive ->
    let closure = transitive_dependency_closure_without_test t immediate_deps in
    Package_name.Set.remove closure package
;;

let non_test_dependencies t package ~traverse =
  check_contains_package t package;
  match traverse with
  | `Immediate ->
    concrete_dependencies_of_local_package t package ~with_test:false
    |> User_error.ok_exn
    |> Package_name.Set.of_list
  | `Transitive ->
    let closure =
      transitive_dependency_closure_without_test t (Package_name.Set.singleton package)
    in
    Package_name.Set.remove closure package
;;

let test_only_dependencies t package ~traverse =
  Package_name.Set.diff
    (all_dependencies t package ~traverse)
    (non_test_dependencies t package ~traverse)
;;

let opam_package_dependencies_of_package t package ~which ~traverse =
  let get_deps =
    match which with
    | `All -> all_dependencies
    | `Non_test -> non_test_dependencies
    | `Test_only -> test_only_dependencies
  in
  get_deps t package ~traverse
  |> Package_name.Set.to_list_map ~f:(fun package_name ->
    OpamPackage.create
      (Package_name.to_opam_package_name package_name)
      (Package_name.Map.find_exn t.version_by_package_name package_name
       |> Package_version.to_opam_package_version))
;;

let opam_package_of_package t package_name =
  check_contains_package t package_name;
  OpamPackage.create
    (Package_name.to_opam_package_name package_name)
    (Package_name.Map.find_exn t.version_by_package_name package_name
     |> Package_version.to_opam_package_version)
;;
