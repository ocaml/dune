open Stdune

let name = Package_name.of_string "dune"

let version =
  let major, minor = Dune_lang.Stanza.latest_version in
  OpamPackage.Version.of_string @@ sprintf "%d.%d" major minor
;;

let package = OpamPackage.create (Package_name.to_opam_package_name name) version
let opam_file = OpamFile.OPAM.create package
