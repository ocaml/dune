open! Import
module Relop = Dune_lang.Relop

let apply_filter env ~with_test (opam_filtered_formula : OpamTypes.filtered_formula)
  : OpamTypes.formula
  =
  OpamFilter.gen_filter_formula
    (OpamFormula.partial_eval (function
      | OpamTypes.Filter flt ->
        `Formula (Atom (OpamTypes.Filter (OpamFilter.partial_eval env flt)))
      | Constraint (relop, filter) ->
        let filter = OpamFilter.partial_eval env filter in
        `Formula (Atom (Constraint (relop, filter)))))
    opam_filtered_formula
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
             (* CR-rgrinberg: shouldn't we accumulate all these errors? *)
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

let filtered_formula_to_package_names env ~with_test version_by_package_name formula =
  formula_to_package_names version_by_package_name (apply_filter ~with_test env formula)
;;
