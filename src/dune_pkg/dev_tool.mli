type t =
  { name : Package_name.t
  ; dependencies : Package_dependency.t list
  }

module Ocamlformat : sig
  val package_dev : Local_package.t
  val program : string
  val pkg_name : string
end

val pkg_tools : string list
val pkg_of_binary : string -> string option
