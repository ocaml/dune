open! Import
module Op = Dune_lang.Package_constraint.Op

let substitute_variables_in_filter
  ~stats_updater
  (solver_env : Solver_env.t)
  (opam_filter : OpamTypes.filter)
  : OpamTypes.filter
  =
  OpamFilter.map_up
    (function
      | FIdent ([], variable, None) as filter ->
        (match Solver_env.Variable.of_string_opt (OpamVariable.to_string variable) with
         | None -> filter
         | Some variable ->
           Option.iter stats_updater ~f:(fun stats_updater ->
             Solver_stats.Updater.expand_variable stats_updater variable);
           (match Solver_env.get solver_env variable with
            | Unset_sys -> filter
            | String string -> FString string))
      | other -> other)
    opam_filter
;;

let apply_filter
  ~stats_updater
  ~with_test
  (solver_env : Solver_env.t)
  (opam_filtered_formula : OpamTypes.filtered_formula)
  : OpamTypes.formula
  =
  let map_filters ~f =
    OpamFilter.gen_filter_formula
      (OpamFormula.partial_eval (function
        | OpamTypes.Filter flt -> `Formula (Atom (OpamTypes.Filter (f flt)))
        | Constraint _ as constraint_ -> `Formula (Atom constraint_)))
  in
  map_filters
    opam_filtered_formula
    ~f:(substitute_variables_in_filter ~stats_updater solver_env)
  |> OpamFilter.filter_deps
       ~build:true
       ~post:false
       ~dev:false
       ~default:false
       ~test:with_test
       ~doc:false
;;

module Version_constraint = struct
  type t = Op.t * Package_version.t

  let of_opam ((relop, version) : OpamFormula.version_constraint) =
    let op =
      match relop with
      | `Eq -> Op.Eq
      | `Neq -> Neq
      | `Geq -> Gte
      | `Gt -> Gt
      | `Leq -> Lte
      | `Lt -> Lt
    in
    let package_version = Package_version.of_opam_package_version version in
    op, package_version
  ;;

  let to_dyn (op, package_version) =
    Dyn.Tuple [ Op.to_dyn op; Package_version.to_dyn package_version ]
  ;;

  let to_string (op, package_version) =
    sprintf "%s %s" (Op.to_string op) (Package_version.to_string package_version)
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

let formula_to_package_names version_by_package_name opam_formula =
  let dnf =
    (* a list of conjunctions where each conjunction represents on set of
       packages with version constrainst that completely satisfies the
       dependency formula *)
    OpamFormula.to_dnf opam_formula
  in
  if List.is_empty dnf
  then (* there are no dependencies so the formula is automatically satisfied *)
    Ok []
  else (
    let satisfied_or_error_hint_per_conjunction =
      (* check if each conjunction can be satisfied and if they can't, produce a hint indicating why it wasn't satisfied *)
      List.map dnf ~f:(fun conjunction ->
        List.map conjunction ~f:(fun (opam_package_name, version_constraint_opt) ->
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
                 Error
                   (Unsatisfied_formula_hint.Unsatisfied_version_constraint
                      { package_name
                      ; found_version = version
                      ; version_constraint = Version_constraint.of_opam constraint_
                      })))
        |> Result.List.all
        |> Result.map ~f:(fun (_ : unit list) -> conjunction))
    in
    match List.find_map satisfied_or_error_hint_per_conjunction ~f:Result.to_option with
    | None ->
      let hints =
        List.filter_map satisfied_or_error_hint_per_conjunction ~f:(function
          | Error hint -> Some hint
          | Ok _ -> None)
      in
      Error (`Formula_could_not_be_satisfied hints)
    | Some satisfied_conjunction ->
      Ok
        (List.map satisfied_conjunction ~f:(fun (opam_package_name, _) ->
           Package_name.of_opam_package_name opam_package_name)))
;;

let filtered_formula_to_package_names
  ~stats_updater
  ~with_test
  solver_env
  version_by_package_name
  formula
  =
  formula_to_package_names
    version_by_package_name
    (apply_filter ~stats_updater ~with_test solver_env formula)
;;
