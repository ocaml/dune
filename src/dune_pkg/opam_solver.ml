open Stdune

module type CONTEXT = Opam_0install.S.CONTEXT

(* Helper functor which implements [CONTEXT] for [(L.t, R.t) Either.t] where
   [L] and [R] both implement [CONTEXT] *)
module Context_either (L : CONTEXT) (R : CONTEXT) :
  CONTEXT with type t = (L.t, R.t) Either.t = struct
  type t = (L.t, R.t) Either.t

  type rejection = (L.rejection, R.rejection) Either.t

  let pp_rejection f = function
    | Left l -> L.pp_rejection f l
    | Right r -> R.pp_rejection f r

  let candidates t name =
    let convert_rejections ~f =
      List.map ~f:(fun (version, result) ->
          (version, Result.map_error ~f result))
    in
    match t with
    | Left l -> L.candidates l name |> convert_rejections ~f:Either.left
    | Right r -> R.candidates r name |> convert_rejections ~f:Either.right

  let user_restrictions = function
    | Left l -> L.user_restrictions l
    | Right r -> R.user_restrictions r

  let filter_deps = function
    | Left l -> L.filter_deps l
    | Right r -> R.filter_deps r
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

  let filter_deps t = Base_context.filter_deps t.base_context
end

module Context_for_dune = struct
  module Dir_context = Opam_0install.Dir_context
  module Switch_context = Opam_0install.Switch_context
  include
    Context_with_local_packages (Context_either (Dir_context) (Switch_context))

  let create_dir_context ~prefer_oldest ~env ~packages_dir_path ~local_packages
      =
    let dir_context =
      Dir_context.create ~prefer_oldest ~constraints:OpamPackage.Name.Map.empty
        ~env packages_dir_path
    in
    create ~base_context:(Left dir_context) ~local_packages

  let create_switch_context ~prefer_oldest ~switch_state ~local_packages =
    let switch_context =
      Switch_context.create ~prefer_oldest
        ~constraints:OpamPackage.Name.Map.empty switch_state
    in
    create ~base_context:(Right switch_context) ~local_packages
end
