open! Import

type t =
  { name : Package_name.t
  ; version : Package_version.t option
  ; dependencies : Package_dependency.t list
  }

let to_opam_file { name; version; dependencies } =
  OpamFile.OPAM.empty
  |> OpamFile.OPAM.with_name (Package_name.to_opam_package_name name)
  |> OpamFile.OPAM.with_version_opt
       (Option.map version ~f:Package_version.to_opam_package_version)
  |> OpamFile.OPAM.with_depends
       (Package_dependency.list_to_opam_filtered_formula dependencies)
;;
