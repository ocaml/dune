(** codept management *)
open Import

val codept_extension : unit Dune_project.Extension.t

include Dep_gen.S
