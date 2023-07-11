open Stdune

module Context_for_dune : sig
  include Opam_0install.S.CONTEXT

  val create_dir_context :
       solver_env:Solver_env.t
    -> packages_dir_path:Filename.t
    -> local_packages:OpamFile.OPAM.t OpamTypes.name_map
    -> version_preference:Version_preference.t
    -> t
end
