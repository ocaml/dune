open Stdune
include String

include (
  Dune_util.Stringlike.Make (struct
    type t = string

    let to_string x = x
    let module_ = "Package_version.Name"
    let description = "package version name"
    let description_of_valid_string = None
    let hint_valid = None
    let of_string_opt s = if s = "" then None else Some s
  end) :
    Dune_util.Stringlike with type t := t)

let of_opam = OpamPackage.Version.to_string
let to_opam = OpamPackage.Version.of_string
