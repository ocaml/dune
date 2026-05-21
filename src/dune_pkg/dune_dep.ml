open Stdune

let name = Package_name.of_string "dune"

let version =
  let major, minor = Dune_lang.Stanza.latest_version in
  OpamPackage.Version.of_string @@ sprintf "%d.%d" major minor
;;

let package = OpamPackage.create (Package_name.to_opam_package_name name) version
let opam_file = OpamFile.OPAM.create package

let strip_upper_version_constraints
  : OpamTypes.filtered_formula -> OpamTypes.filtered_formula
  =
  let dune_opam_name = Package_name.to_opam_package_name name in
  OpamFormula.map (fun (atom_name, condition) ->
    if OpamPackage.Name.equal atom_name dune_opam_name
    then (
      let condition =
        OpamFormula.map
          (function
            | OpamTypes.Filter _ as f -> OpamFormula.Atom f
            | Constraint (relop, _) as c ->
              (match relop with
               | `Lt | `Leq -> OpamFormula.Empty
               | `Eq | `Neq | `Gt | `Geq -> OpamFormula.Atom c))
          condition
      in
      OpamFormula.Atom (atom_name, condition))
    else OpamFormula.Atom (atom_name, condition))
;;
