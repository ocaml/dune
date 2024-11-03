open Import

type t = OpamTypes.filtered_formula

let of_dependencies deps = Package_dependency.list_to_opam_filtered_formula deps
let to_filtered_formula v = v
let of_filtered_formula v = v
let to_dyn = Opam_dyn.filtered_formula

let rec union = function
  | [] -> OpamTypes.Empty
  | x :: xs ->
    let xs = union xs in
    OpamTypes.And (x, xs)
;;

let rec remove_packages (v : OpamTypes.filtered_formula) pkgs =
  match v with
  | Empty -> OpamTypes.Empty
  | Block b -> Block (remove_packages b pkgs)
  | Atom (name, _condition) as a ->
    let name = name |> OpamPackage.Name.to_string |> Package_name.of_string in
    (match Package_name.Set.mem pkgs name with
     | true -> Empty
     | false -> a)
  | And (l, r) ->
    let l = remove_packages l pkgs in
    let r = remove_packages r pkgs in
    And (l, r)
  | Or (l, r) ->
    let l = remove_packages l pkgs in
    let r = remove_packages r pkgs in
    Or (l, r)
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
