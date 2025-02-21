open Import

type t = OpamTypes.filtered_formula

let of_dependencies deps =
  List.map deps ~f:Package_dependency.to_opam_filtered_formula |> OpamFormula.ands
;;

let to_filtered_formula v = v
let of_filtered_formula v = v
let to_dyn = Opam_dyn.filtered_formula
let ands = OpamFormula.ands

let remove_packages (v : OpamTypes.filtered_formula) pkgs =
  OpamFormula.map_up_formula
    (function
      | Atom (name, _condition) as a ->
        if
          let name = Package_name.of_opam_package_name name in
          Package_name.Set.mem pkgs name
        then Empty
        else a
      | x -> x)
    v
;;

exception Found of Package_name.t

let any_package_name (v : OpamTypes.filtered_formula) =
  try
    OpamFormula.iter
      (fun (name, _condition) ->
         let name = Package_name.of_opam_package_name name in
         raise_notrace (Found name))
      v;
    None
  with
  | Found name -> Some name
;;

let has_entries v = v |> any_package_name |> Option.is_some
