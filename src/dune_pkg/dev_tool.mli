open Import

type t =
  { name : Package_name.t
  ; dependencies : Package_dependency.t list
  }

module Ocamlformat : sig
  val package_dev : Local_package.t
  val program : string
  val pkg_name : string
  val lock_dir : Path.Source.t
end
