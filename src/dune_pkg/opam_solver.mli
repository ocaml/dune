open Stdune

module Context_for_dune : sig
  include Opam_0install.S.CONTEXT

  val create_dir_context :
       env:(string -> OpamVariable.variable_contents option)
    -> packages_dir_path:Filename.t
    -> local_packages:OpamFile.OPAM.t OpamTypes.name_map
    -> prefer_oldest:bool
    -> t

  val create_switch_context :
       switch_state:OpamStateTypes.unlocked OpamStateTypes.switch_state
    -> local_packages:OpamFile.OPAM.t OpamTypes.name_map
    -> prefer_oldest:bool
    -> t
end
