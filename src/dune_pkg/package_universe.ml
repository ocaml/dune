open Import

type t =
  { local_packages : Local_package.t Package_name.Map.t
  ; lock_dir : Lock_dir.t
  ; platform : Solver_env.t
  ; lock_packages : Lock_dir.Pkg.t Package_name.Map.t
  ; version_by_package_name : Package_version.t Package_name.Map.t
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

let version_by_package_name local_packages lock_packages =
  let from_local_packages =
    Package_name.Map.map local_packages ~f:(fun (local_package : Local_package.t) ->
      local_package.version)
  in
  let from_lock_dir =
    Package_name.Map.map lock_packages ~f:(fun (pkg : Lock_dir.Pkg.t) -> pkg.info.version)
  in
  let exception Duplicate_package of Package_name.t in
  try
    Package_name.Map.union
      from_local_packages
      from_lock_dir
      ~f:(fun duplicate_package_name _ _ ->
        raise (Duplicate_package duplicate_package_name))
  with
  | Duplicate_package duplicate_package_name ->
    let local_package = Package_name.Map.find_exn local_packages duplicate_package_name in
    User_error.raise
      ~hints:lockdir_regenerate_hints
      ~loc:local_package.loc
      [ Pp.textf
          "A package named %S is defined locally but is also present in the lockdir"
          (Package_name.to_string local_package.name)
      ]
;;

let concrete_dependencies_of_local_package t local_package_name ~with_test =
  let local_package = Package_name.Map.find_exn t.local_packages local_package_name in
  let env =
    Solver_stats.Expanded_variable_bindings.to_solver_env
      t.lock_dir.expanded_solver_variable_bindings
    |> Solver_env.to_env
  in
  match
    Lock_pkg.local_package_dependencies
      (Local_package.for_solver local_package)
      ~env
      ~with_test
      ~packages:t.version_by_package_name
      ~dune_version:(Package_version.of_opam_package_version Dune_dep.version)
  with
  | Ok regular -> regular
  | Error (`Formula_could_not_be_satisfied unsatisfied_formula_hints) ->
    User_error.raise
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
            ~f:Resolve_opam_formula.Unsatisfied_formula_hint.pp)
;;

let all_non_local_dependencies_of_local_packages t =
  let all_dependencies_of_local_packages =
    Package_name.Map.keys t.local_packages
    |> List.map ~f:(fun p ->
      concrete_dependencies_of_local_package ~with_test:true t p
      |> Package_name.Set.of_list)
    |> Package_name.Set.union_all
  in
  Package_name.Set.diff
    all_dependencies_of_local_packages
    (Package_name.Set.of_keys t.local_packages)
;;

let dependency_digest local_packages =
  let local_packages =
    Package_name.Map.values local_packages |> List.map ~f:Local_package.for_solver
  in
  Local_package.For_solver.non_local_dependencies local_packages
  |> Local_package.Dependency_hash.of_dependency_formula
;;

let up_to_date local_packages ~dependency_hash:saved_dependency_hash =
  let dependency_hash = dependency_digest local_packages in
  match saved_dependency_hash, dependency_hash with
  | None, None -> `Valid
  | Some lock_dir_dependency_hash, Some non_local_dependencies_hash
    when Local_package.Dependency_hash.equal
           lock_dir_dependency_hash
           non_local_dependencies_hash -> `Valid
  | None, Some _ ->
    `Valid (* This case happens when the user writes themselves their lock.dune. *)
  | Some _, Some _ -> `Invalid
  | Some _, None -> `Invalid
;;

let validate_dependency_hash local_packages ~saved_dependency_hash =
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
  let dependency_hash =
    Local_package.For_solver.non_local_dependencies local_packages
    |> Local_package.Dependency_hash.of_dependency_formula
  in
  match saved_dependency_hash, dependency_hash with
  | None, None -> ()
  | Some (loc, lock_dir_dependency_hash), None ->
    User_error.raise
      ~loc
      ~hints:regenerate_lock_dir_hints
      [ Pp.textf
          "This project has no non-local dependencies yet the lockfile contains a \
           dependency hash: %s"
          (Local_package.Dependency_hash.to_string lock_dir_dependency_hash)
      ]
  | None, Some _ ->
    let any_non_local_dependency_name =
      let non_local_dependencies =
        Local_package.For_solver.non_local_dependencies local_packages
      in
      match Dependency_formula.any_package_name non_local_dependencies with
      | Some x -> x
      | None ->
        Code_error.raise
          "Attempting to retrieve a non-local dependency but there aren't any"
          []
    in
    User_error.raise
      ~hints:regenerate_lock_dir_hints
      [ Pp.text
          "This project has at least one non-local dependency but the lockdir doesn't \
           contain a dependency hash."
      ; Pp.textf
          "An example of a non-local dependency of this project is: %s"
          (Package_name.to_string any_non_local_dependency_name)
      ]
  | Some (loc, lock_dir_dependency_hash), Some non_local_dependency_hash ->
    if
      Local_package.Dependency_hash.equal
        lock_dir_dependency_hash
        non_local_dependency_hash
    then ()
    else
      User_error.raise
        ~loc
        ~hints:regenerate_lock_dir_hints
        [ Pp.text
            "Dependency hash in lockdir does not match the hash of non-local \
             dependencies of this project. The lockdir expects the non-local \
             dependencies to hash to:"
        ; Pp.text (Local_package.Dependency_hash.to_string lock_dir_dependency_hash)
        ; Pp.text "...but the non-local dependencies of this project hash to:"
        ; Pp.text (Local_package.Dependency_hash.to_string non_local_dependency_hash)
        ]
;;

let make ~platform local_packages lock_dir =
  let lock_packages =
    Lock_dir.Packages.pkgs_on_platform_by_name ~platform lock_dir.Lock_dir.packages
  in
  let version_by_package_name = version_by_package_name local_packages lock_packages in
  { local_packages; lock_dir; platform; lock_packages; version_by_package_name }
;;

(* The immediate dependencies of a node of the combined workspace/lockdir
   dependency graph. Local packages contribute the concrete dependencies of
   their formula; lock packages contribute the dependencies declared for the
   current platform. Names that resolve to neither kind of package are
   leaves: for valid input they can only be dune or a package disabled on
   the current platform. *)
let immediate_build_dependencies (t : t) package_name =
  match Package_name.Map.find t.local_packages package_name with
  | Some _ -> concrete_dependencies_of_local_package t package_name ~with_test:false
  | None ->
    (match Package_name.Map.find t.lock_packages package_name with
     | None -> []
     | Some package ->
       Lock_dir.Conditional_choice.choose_for_platform
         package.depends
         ~platform:t.platform
       |> Option.value ~default:[]
       |> List.map ~f:(fun (dependency : Lock_dir.Dependency.t) -> dependency.name))
;;

let transitive_dependency_closure_without_test t start =
  let rec loop seen = function
    | [] -> seen
    | name :: names ->
      if Package_name.Set.mem seen name
      then loop seen names
      else
        loop (Package_name.Set.add seen name) (immediate_build_dependencies t name @ names)
  in
  loop Package_name.Set.empty (Package_name.Set.to_list start)
;;

let contains_package t package_name =
  let in_local_packages = Package_name.Map.mem t.local_packages package_name in
  let in_lock_dir = Package_name.Map.mem t.lock_packages package_name in
  in_local_packages || in_lock_dir
;;

let check_lock_packages_do_not_depend_on_local_packages t =
  Package_name.Map.iter
    t.lock_packages
    ~f:(fun { Lock_dir.Pkg.depends; info = { name = package_name; _ }; _ } ->
      Lock_dir.Conditional_choice.choose_for_platform depends ~platform:t.platform
      |> Option.value ~default:[]
      |> List.iter ~f:(fun { Lock_dir.Dependency.name = dependency_name; loc } ->
        if
          (not (Package_name.equal dependency_name Dune_dep.name))
          && Package_name.Map.mem t.local_packages dependency_name
        then
          User_error.raise
            ~loc
            [ Pp.textf
                "Dune does not support packages outside the workspace depending on \
                 packages in the workspace. The package %S is not in the workspace but \
                 it depends on the package %S which is in the workspace."
                (Package_name.to_string package_name)
                (Package_name.to_string dependency_name)
            ]))
;;

let check_for_unnecessary_packges_in_lock_dir
      t
      all_non_local_dependencies_of_local_packages
  =
  let unneeded_packages_in_lock_dir =
    let locked_transitive_closure_of_local_package_dependencies =
      transitive_dependency_closure_without_test
        t
        all_non_local_dependencies_of_local_packages
    in
    Package_name.Set.diff
      (Package_name.Set.of_keys t.lock_packages)
      locked_transitive_closure_of_local_package_dependencies
  in
  if Package_name.Set.is_empty unneeded_packages_in_lock_dir
  then ()
  else (
    let packages =
      Package_name.Set.to_list unneeded_packages_in_lock_dir
      |> List.map ~f:(Package_name.Map.find_exn t.lock_packages)
    in
    User_error.raise
      ~hints:lockdir_regenerate_hints
      [ Pp.text
          "The lockdir contains packages which are not among the transitive dependencies \
           of any local package:"
      ; Pp.enumerate packages ~f:(fun (package : Lock_dir.Pkg.t) ->
          Pp.textf
            "%s.%s"
            (Package_name.to_string package.info.name)
            (Package_version.to_string package.info.version))
      ])
;;

let validate t =
  validate_dependency_hash
    t.local_packages
    ~saved_dependency_hash:t.lock_dir.dependency_hash;
  check_lock_packages_do_not_depend_on_local_packages t;
  all_non_local_dependencies_of_local_packages t
  |> check_for_unnecessary_packges_in_lock_dir t
;;

let create ~platform local_packages lock_dir =
  try
    let t = make ~platform local_packages lock_dir in
    validate t;
    Ok t
  with
  | User_error.E e -> Error e
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
    | x -> Package_name.Set.of_list x
    | exception User_error.E e ->
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
