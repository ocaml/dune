(** codept management *)
open Import

type t

val extension : t Dune_project.Extension.t

include Dep_gen.S
