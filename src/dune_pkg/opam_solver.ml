open Stdune

module type CONTEXT = Opam_0install.S.CONTEXT

(* Helper functor which implements [CONTEXT] for [(L.t, R.t) Either.t] where
   [L] and [R] both implement [CONTEXT] *)
module Context_either (L : CONTEXT) (R : CONTEXT) :
  CONTEXT with type t = (L.t, R.t) Either.t = struct
  type t = (L.t, R.t) Either.t

  type rejection = (L.rejection, R.rejection) Either.t

  let pp_rejection f = Either.map ~l:(L.pp_rejection f) ~r:(R.pp_rejection f)

  let candidates =
    let convert_rejections ~f =
      List.map ~f:(fun (version, result) ->
          (version, Result.map_error ~f result))
    in
    Either.map
      ~l:(fun l name ->
        L.candidates l name |> convert_rejections ~f:Either.left)
      ~r:(fun r name ->
        R.candidates r name |> convert_rejections ~f:Either.right)

  let user_restrictions =
    Either.map ~l:L.user_restrictions ~r:R.user_restrictions

  let filter_deps = Either.map ~l:L.filter_deps ~r:R.filter_deps
end

(* Helper module for working with [OpamTypes.filtered_formula] *)
module Filtered_formula : sig
  open OpamTypes

  (** Update a filtered formula setting the "with-test" and "with-doc" variables
      to true. *)
  val with_test_and_doc : filtered_formula -> filtered_formula
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

  let with_test_and_doc filtered_formula =
    List.fold_left [ "with-test"; "with-doc" ] ~init:filtered_formula
      ~f:(fun acc variable_name ->
        resolve_simple_bool_variable acc
          (OpamVariable.of_string variable_name)
          true)
end

(* Helper functor which adds a set of local packages to a [CONTEXT] *)
module Context_with_local_packages (Base_context : CONTEXT) : sig
  include CONTEXT

  val create :
       base_context:Base_context.t
    -> local_packages:OpamFile.OPAM.t OpamPackage.Name.Map.t
    -> t
end = struct
  let local_package_default_version = OpamPackage.Version.of_string "LOCAL"

  type t =
    { base_context : Base_context.t
    ; local_packages : OpamFile.OPAM.t OpamPackage.Name.Map.t
    }

  let create ~base_context ~local_packages = { base_context; local_packages }

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
        Filtered_formula.with_test_and_doc filtered_formula
      else filtered_formula
    in
    Base_context.filter_deps t.base_context package filtered_formula
end

module Context_for_dune = struct
  module Dir_context = Opam_0install.Dir_context
  module Switch_context = Opam_0install.Switch_context
  include
    Context_with_local_packages (Context_either (Dir_context) (Switch_context))

  let create_dir_context ~env ~packages_dir_path ~local_packages ~prefer_oldest
      =
    let dir_context =
      Dir_context.create ~prefer_oldest ~constraints:OpamPackage.Name.Map.empty
        ~env packages_dir_path
    in
    create ~base_context:(Left dir_context) ~local_packages

  let create_switch_context ~switch_state ~local_packages ~prefer_oldest =
    let switch_context =
      Switch_context.create ~prefer_oldest
        ~constraints:OpamPackage.Name.Map.empty switch_state
    in
    create ~base_context:(Right switch_context) ~local_packages
end
