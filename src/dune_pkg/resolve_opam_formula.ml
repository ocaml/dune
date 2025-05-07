open! Import
module Relop = Dune_lang.Relop

let apply_filter env ~with_test ~(formula : OpamTypes.filtered_formula)
  : OpamTypes.formula
  =
  OpamFilter.gen_filter_formula
    (OpamFormula.partial_eval (fun (form : _ OpamTypes.filter_or_constraint) ->
       match form with
       | Filter flt ->
         `Formula (Atom (OpamTypes.Filter (OpamFilter.partial_eval env flt)))
       | Constraint (relop, filter) ->
         let filter = OpamFilter.partial_eval env filter in
         `Formula (Atom (Constraint (relop, filter)))))
    formula
  |> OpamFilter.filter_deps
       ~build:true
       ~post:false
       ~dev:false
       ~default:false
       ~test:with_test
       ~doc:false
;;

module Version_constraint = struct
  type t = Relop.t * Package_version.t

  let of_opam ((relop, version) : OpamFormula.version_constraint) =
    let package_version = Package_version.of_opam_package_version version in
    Package_dependency.Constraint.Op.of_opam relop, package_version
  ;;

  let to_dyn (op, package_version) =
    Dyn.Tuple [ Relop.to_dyn op; Package_version.to_dyn package_version ]
  ;;

  let to_string (op, package_version) =
    sprintf "%s %s" (Relop.to_string op) (Package_version.to_string package_version)
  ;;
end

module Unsatisfied_formula_hint = struct
  type t =
    | Missing_package of Package_name.t
    | Unsatisfied_version_constraint of
        { package_name : Package_name.t
        ; found_version : Package_version.t
        ; version_constraint : Version_constraint.t
        }

  let to_dyn = function
    | Missing_package package_name ->
      Dyn.variant "Missing_package" [ Package_name.to_dyn package_name ]
    | Unsatisfied_version_constraint { package_name; found_version; version_constraint }
      ->
      Dyn.variant
        "Unsatisfied_version_constraint"
        [ Dyn.record
            [ "package_name", Package_name.to_dyn package_name
            ; "found_version", Package_version.to_dyn found_version
            ; "version_constraint", Version_constraint.to_dyn version_constraint
            ]
        ]
  ;;

  let pp = function
    | Missing_package missing_package ->
      Pp.textf "Package %S is missing" (Package_name.to_string missing_package)
    | Unsatisfied_version_constraint { package_name; found_version; version_constraint }
      ->
      Pp.textf
        "Found version %S of package %S which doesn't satisfy the required version \
         constraint %S"
        (Package_version.to_string found_version)
        (Package_name.to_string package_name)
        (Version_constraint.to_string version_constraint)
  ;;
end

type unsatisfied_formula =
  [ `Formula_could_not_be_satisfied of Unsatisfied_formula_hint.t list ]

let dedup_package_names package_names =
  let ret_reversed, _ =
    List.fold_left
      package_names
      ~init:([], Package_name.Set.empty)
      ~f:(fun (ret_reversed, seen) x ->
        if Package_name.Set.mem seen x
        then ret_reversed, seen
        else x :: ret_reversed, Package_name.Set.add seen x)
  in
  List.rev ret_reversed
;;

let formula_to_package_names version_by_package_name opam_formula =
  let check_conjunction conjunction =
    Result.List.map conjunction ~f:(fun (opam_package_name, version_constraint_opt) ->
      let package_name = Package_name.of_opam_package_name opam_package_name in
      match Package_name.Map.find version_by_package_name package_name with
      | None ->
        (* this package wasn't part of the solution so the current
           conjunction can't be satisfied *)
        Error (Unsatisfied_formula_hint.Missing_package package_name)
      | Some version ->
        (match version_constraint_opt with
         | None ->
           (* no constraint so any version will satisfy *)
           Ok ()
         | Some (constraint_ : OpamFormula.version_constraint) ->
           let opam_version = Package_version.to_opam_package_version version in
           let version_formula = OpamFormula.Atom constraint_ in
           if OpamFormula.check_version_formula version_formula opam_version
           then Ok ()
           else
             (* CR rgrinberg: shouldn't we accumulate all these errors? *)
             Error
               (Unsatisfied_formula_hint.Unsatisfied_version_constraint
                  { package_name
                  ; found_version = version
                  ; version_constraint = Version_constraint.of_opam constraint_
                  })))
    |> Result.map ~f:(fun (_ : unit list) -> ())
  in
  let satisfied_or_error_hint_per_conjunction =
    (* check if each conjunction can be satisfied and if they can't, produce
       a hint indicating why it wasn't satisfied *)
    let rec loop errors = function
      | [] ->
        (match errors with
         | [] -> Ok []
         | _ :: _ -> Error (List.rev errors))
      | conjunction :: xs ->
        (match check_conjunction conjunction with
         | Ok () -> Ok conjunction
         | Error e -> loop (e :: errors) xs)
    in
    (* a list of conjunctions where each conjunction represents on set of
       packages with version constraint that completely satisfies the
       dependency formula *)
    OpamFormula.to_dnf opam_formula |> loop []
  in
  match satisfied_or_error_hint_per_conjunction with
  | Error hints -> Error (`Formula_could_not_be_satisfied hints)
  | Ok satisfied_conjunction ->
    let package_names_from_conjunctions =
      List.map satisfied_conjunction ~f:(fun (opam_package_name, _) ->
        Package_name.of_opam_package_name opam_package_name)
    in
    let package_names =
      (* If a package name appeared in multiple conjunctions then it
         will be duplicated in [package_names_from_conjunctions]. *)
      dedup_package_names package_names_from_conjunctions
    in
    Ok package_names
;;

(* Override the setting of the "post" variable to a given boolean
   value by passing [Some value], or force it to be unset by passing
   [None]. *)
let override_post post_value env var =
  match OpamVariable.Full.scope var with
  | Global
    when Package_variable_name.equal
           (Package_variable_name.of_opam @@ OpamVariable.Full.variable var)
           Package_variable_name.post -> Option.map post_value ~f:(fun b -> OpamTypes.B b)
  | _ -> env var
;;

(* Check that a package version satisfies the version constraint
   associated with a package dependency in an opam file. *)
let package_version_satisfies_opam_version_constraint_opt
      package_version
      opam_version_constraint_opt
  =
  match opam_version_constraint_opt with
  | None -> true
  | Some (constraint_ : OpamFormula.version_constraint) ->
    let opam_version = Package_version.to_opam_package_version package_version in
    let version_formula = OpamFormula.Atom constraint_ in
    OpamFormula.check_version_formula version_formula opam_version
;;

(* TODO (steve): This does a very similar thing to
   [formula_to_package_names] so the two functions should be
   combined. *)
let formula_to_package_names_allow_missing version_by_package_name opam_formula =
  let cnf_terms = OpamFormula.to_cnf opam_formula in
  List.filter_map cnf_terms ~f:(fun disjunction ->
    (* Take the first term of the disjunction that is part of the set of packages in the
       solution, if any. *)
    List.find_map disjunction ~f:(fun (opam_package_name, version_constraint_opt) ->
      let package_name = Package_name.of_opam_package_name opam_package_name in
      Package_name.Map.find version_by_package_name package_name
      |> Option.bind ~f:(fun version_in_solution ->
        if
          package_version_satisfies_opam_version_constraint_opt
            version_in_solution
            version_constraint_opt
        then Some package_name
        else None)))
;;

type deps =
  { post : Package_name.t list
  ; regular : Package_name.t list
  }

let filtered_formula_to_package_names ~env ~with_test ~packages formula =
  let open Result.O in
  let+ all = apply_filter ~with_test env ~formula |> formula_to_package_names packages in
  let regular, post =
    let regular_set =
      override_post (Some false) env
      |> apply_filter ~with_test ~formula
      |> formula_to_package_names_allow_missing packages
      |> Package_name.Set.of_list
    in
    List.partition all ~f:(Package_name.Set.mem regular_set)
  in
  { regular; post }
;;
