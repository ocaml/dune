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

let rec remove_packages v pkgs =
  match (v : OpamTypes.filtered_formula) with
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

let rec any_package_name v =
  match (v : OpamTypes.filtered_formula) with
  | Empty -> None
  | Block b -> any_package_name b
  | Atom (name, _condition) ->
    let name = name |> OpamPackage.Name.to_string |> Package_name.of_string in
    Some name
  | And (l, r) | Or (l, r) ->
    (match any_package_name l with
     | Some _ as r -> r
     | None -> any_package_name r)
;;

let has_entries v = v |> any_package_name |> Option.is_some
