(** codept management *)
open Import

type t

val codept_extension : t Dune_project.Extension.t

include Dep_gen.S
