open! Stdune
include Dune_lang.Package_name

let of_opam_package_name opam_package_name =
  OpamPackage.Name.to_string opam_package_name |> of_string
;;

let to_opam_package_name t = to_string t |> OpamPackage.Name.of_string
