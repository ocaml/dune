open! Stdune
include Dune_lang.Package_version

let of_opam opam_version = OpamPackage.Version.to_string opam_version |> of_string
let to_opam t = to_string t |> OpamPackage.Version.of_string
