open Stdune

module type CONTEXT = Opam_0install.S.CONTEXT

(* Helper module for working with [OpamTypes.filtered_formula] *)
module Filtered_formula : sig
  open OpamTypes

  (** Update a filtered formula setting the "with-test" and "with-doc" variables
      to true. *)
  val with_flags : Solver_env.Flag.Set.t -> filtered_formula -> filtered_formula
end = struct
  open OpamTypes

  (* [map_formula_filters_bottom_up t ~f] calls [f] on each node of each filter
     in t, bottom-up *)
  let map_filters_bottom_up ~f =
    OpamFilter.gen_filter_formula
      (OpamFormula.partial_eval (function
        | Filter flt -> `Formula (Atom (Filter (OpamFilter.map_up f flt)))
        | Constraint _ as constraint_ -> `Formula (Atom constraint_)))

  (* update a filtered formula replacing instances of a given variable with a given boolean value *)
  let resolve_simple_bool_variable filtered_formula variable_to_resolve value =
    map_filters_bottom_up filtered_formula ~f:(function
      | FIdent ([], variable, None) as filter ->
        if OpamVariable.equal variable variable_to_resolve then FBool value
        else filter
      | other -> other)

  let with_flags flags filtered_formula =
    Solver_env.Flag.Set.fold flags ~init:filtered_formula ~f:(fun flag acc ->
        resolve_simple_bool_variable acc
          (OpamVariable.of_string (Solver_env.Flag.to_string flag))
          true)
end

(* Helper functor which adds a set of local packages to a [CONTEXT] *)
module Context_with_local_packages (Base_context : CONTEXT) : sig
  include CONTEXT

  val create :
       base_context:Base_context.t
    -> local_packages:OpamFile.OPAM.t OpamPackage.Name.Map.t
    -> solver_env:Solver_env.t
    -> t
end = struct
  let local_package_default_version =
    OpamPackage.Version.of_string Lock_dir.Pkg_info.default_version

  type t =
    { base_context : Base_context.t
    ; local_packages : OpamFile.OPAM.t OpamPackage.Name.Map.t
    ; solver_env : Solver_env.t
    }

  let create ~base_context ~local_packages ~solver_env =
    { base_context; local_packages; solver_env }

  type rejection = Base_context.rejection

  let pp_rejection = Base_context.pp_rejection

  let candidates t name =
    match OpamPackage.Name.Map.find_opt name t.local_packages with
    | None -> Base_context.candidates t.base_context name
    | Some opam_file ->
      let version =
        Option.value opam_file.version ~default:local_package_default_version
      in
      [ (version, Ok opam_file) ]

  let user_restrictions t = Base_context.user_restrictions t.base_context

  (* Override [filter_deps] so that local packages get test and doc dependencies *)
  let filter_deps t package filtered_formula =
    let package_is_local =
      OpamPackage.Name.Map.mem (OpamPackage.name package) t.local_packages
    in
    let filtered_formula =
      if package_is_local then
        Filtered_formula.with_flags t.solver_env.flags filtered_formula
      else filtered_formula
    in
    Base_context.filter_deps t.base_context package filtered_formula
end

module Context_for_dune = struct
  module Dir_context = Opam_0install.Dir_context
  include Context_with_local_packages (Dir_context)

  let prefer_oldest = function
    | Version_preference.Oldest -> true
    | Newest -> false

  let create_dir_context ~solver_env ~env ~packages_dir_path ~local_packages
      ~version_preference =
    let dir_context =
      Dir_context.create
        ~prefer_oldest:(prefer_oldest version_preference)
        ~constraints:OpamPackage.Name.Map.empty ~env packages_dir_path
    in
    create ~base_context:dir_context ~local_packages ~solver_env
end
