open Import

type t = OpamTypes.filtered_formula

let of_dependencies deps = Package_dependency.list_to_opam_filtered_formula deps
let to_filtered_formula v = v
let of_filtered_formula v = v

let relop_to_sexp = function
  | `Eq -> Dune_sexp.atom "="
  | `Neq -> Dune_sexp.atom "!="
  | `Geq -> Dune_sexp.atom ">="
  | `Gt -> Dune_sexp.atom ">"
  | `Leq -> Dune_sexp.atom "<="
  | `Lt -> Dune_sexp.atom "<"
;;

let rec filter_to_sexp filter =
  match (filter : OpamTypes.filter) with
  | FBool b -> Dune_sexp.atom (sprintf "%B" b)
  | FString s -> Dune_sexp.atom_or_quoted_string s
  | FIdent (names, var, conv) ->
    let names =
      List.map
        ~f:(function
          | None -> Dune_sexp.List []
          | Some n -> n |> OpamPackage.Name.to_string |> Dune_sexp.atom_or_quoted_string)
        names
    in
    let names = Dune_sexp.List names in
    let var = var |> OpamVariable.to_string |> Dune_sexp.atom_or_quoted_string in
    let ident =
      match conv with
      | None -> [ names; var ]
      | Some (a, b) ->
        let a = Dune_sexp.atom_or_quoted_string a in
        let b = Dune_sexp.atom_or_quoted_string b in
        [ names; var; Dune_sexp.List [ a; b ] ]
    in
    Dune_sexp.List ident
  | FOp (l, relop, r) ->
    let relop = relop_to_sexp relop in
    let l = filter_to_sexp l in
    let r = filter_to_sexp r in
    Dune_sexp.List [ relop; l; r ]
  | FAnd (l, r) ->
    let l = filter_to_sexp l in
    let r = filter_to_sexp r in
    Dune_sexp.List [ Dune_sexp.atom "and"; l; r ]
  | FOr (l, r) ->
    let l = filter_to_sexp l in
    let r = filter_to_sexp r in
    Dune_sexp.List [ Dune_sexp.atom "or"; l; r ]
  | FNot filter ->
    let filter = filter_to_sexp filter in
    Dune_sexp.List [ Dune_sexp.atom "not"; filter ]
  | FDefined filter ->
    let filter = filter_to_sexp filter in
    Dune_sexp.List [ Dune_sexp.atom "defined"; filter ]
  | FUndef filter ->
    let filter = filter_to_sexp filter in
    Dune_sexp.List [ Dune_sexp.atom "undefined"; filter ]
;;

let filter_or_constraint_to_sexp foc =
  match (foc : OpamTypes.filter OpamTypes.filter_or_constraint) with
  | Filter filter ->
    let filter = filter_to_sexp filter in
    Dune_sexp.List [ Dune_sexp.atom "filter"; filter ]
  | Constraint (relop, filter) ->
    let relop = relop_to_sexp relop in
    let filter = filter_to_sexp filter in
    Dune_sexp.List [ Dune_sexp.atom "constraint"; Dune_sexp.List [ relop; filter ] ]
;;

let rec condition_to_sexp c =
  match (c : OpamTypes.condition) with
  | Empty -> Dune_sexp.List []
  | Block b -> condition_to_sexp b
  | Atom a -> filter_or_constraint_to_sexp a
  | And (l, r) ->
    let l = condition_to_sexp l in
    let r = condition_to_sexp r in
    Dune_sexp.List [ Dune_sexp.atom "and"; l; r ]
  | Or (l, r) ->
    let l = condition_to_sexp l in
    let r = condition_to_sexp r in
    Dune_sexp.List [ Dune_sexp.atom "or"; l; r ]
;;

let rec to_sexp v =
  match (v : OpamTypes.filtered_formula) with
  | Empty -> Dune_sexp.List []
  | Block b -> to_sexp b
  | Atom (name, condition) ->
    let name = name |> OpamPackage.Name.to_string |> Dune_sexp.atom_or_quoted_string in
    let condition = condition_to_sexp condition in
    Dune_sexp.List [ name; condition ]
  | And (l, r) ->
    let l = to_sexp l in
    let r = to_sexp r in
    Dune_sexp.List [ Dune_sexp.atom "and"; l; r ]
  | Or (l, r) ->
    let l = to_sexp l in
    let r = to_sexp r in
    Dune_sexp.List [ Dune_sexp.atom "or"; l; r ]
;;

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

let rec is_post_filter filter =
  match (filter : OpamTypes.filter) with
  | FBool _ -> false
  | FString _ -> false
  | FIdent (_, var, _) ->
    (match OpamVariable.to_string var with
     | "post" -> true
     | _ -> false)
  | FOp (l, _relop, r) -> is_post_filter l || is_post_filter r
  | FAnd (l, r) | FOr (l, r) -> is_post_filter l || is_post_filter r
  | FNot filter -> not (is_post_filter filter)
  | FDefined filter -> is_post_filter filter
  | FUndef filter -> not (is_post_filter filter)
;;

let is_post_foc filter_or_constraint =
  match (filter_or_constraint : OpamTypes.filter OpamTypes.filter_or_constraint) with
  | Filter filter -> is_post_filter filter
  | Constraint _ -> false
;;

let is_post condition =
  condition |> OpamFormula.formula_to_cnf |> List.exists ~f:(List.exists ~f:is_post_foc)
;;

let reachable_dependencies v =
  let rec loop v =
    match (v : OpamTypes.filtered_formula) with
    | Empty -> Package_name.Set.empty
    | Atom (name, condition) ->
      (match is_post condition with
       | true -> Package_name.Set.empty
       | false ->
         let name = name |> OpamPackage.Name.to_string |> Package_name.of_string in
         Package_name.Set.singleton name)
    | Block b -> loop b
    | And (l, r) | Or (l, r) ->
      let l = loop l in
      let r = loop r in
      Package_name.Set.union l r
  in
  loop v
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

let has_entries v =
  match any_package_name v with
  | None -> false
  | Some _ -> true
;;
