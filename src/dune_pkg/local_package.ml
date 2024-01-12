open! Import
module Package_constraint = Dune_lang.Package_constraint
module Digest = Dune_digest

type t =
  { name : Opam_compatible_package_name.t
  ; version : Package_version.t option
  ; dependencies : Package_dependency.t list
  ; conflicts : Package_dependency.t list
  ; conflict_class : Opam_compatible_package_name.t list
  ; depopts : Package_dependency.t list
  ; loc : Loc.t
  }

module Dependency_hash = struct
  include Digest

  let encode t = to_string t |> Encoder.string

  let decode =
    let open Decoder in
    let+ loc, hash = located string in
    match Digest.from_hex hash with
    | Some hash -> hash
    | None ->
      User_error.raise
        ~loc
        [ Pp.textf "Dependency hash is not a valid md5 hash: %s" hash ]
  ;;
end

module Dependency_set = struct
  type t = Package_constraint.Set.t Opam_compatible_package_name.Map.t

  let empty = Opam_compatible_package_name.Map.empty

  let of_list =
    List.fold_left ~init:empty ~f:(fun acc { Package_dependency.name; constraint_ } ->
      Opam_compatible_package_name.Map.update acc name ~f:(fun existing ->
        match existing, constraint_ with
        | None, None -> Some Package_constraint.Set.empty
        | None, Some constraint_ -> Some (Package_constraint.Set.singleton constraint_)
        | Some existing, None -> Some existing
        | Some existing, Some constraint_ ->
          Some (Package_constraint.Set.add existing constraint_)))
  ;;

  let union =
    Opam_compatible_package_name.Map.union ~f:(fun _name a b ->
      Some (Package_constraint.Set.union a b))
  ;;

  let union_all = List.fold_left ~init:empty ~f:union

  let package_dependencies =
    Opam_compatible_package_name.Map.to_list_map ~f:(fun name constraints ->
      let constraint_ =
        if Package_constraint.Set.is_empty constraints
        then None
        else Some (Package_constraint.And (Package_constraint.Set.to_list constraints))
      in
      { Package_dependency.name; constraint_ })
  ;;

  let encode_for_hash t =
    package_dependencies t |> Dune_lang.Encoder.list Package_dependency.encode
  ;;

  let hash t =
    if Opam_compatible_package_name.Map.is_empty t
    then None
    else Some (encode_for_hash t |> Dune_sexp.to_string |> Dune_digest.string)
  ;;
end

module For_solver = struct
  type t =
    { name : Opam_compatible_package_name.t
    ; dependencies : Package_dependency.t list
    ; conflicts : Package_dependency.t list
    ; depopts : Package_dependency.t list
    ; conflict_class : Opam_compatible_package_name.t list
    }

  let to_opam_file { name; dependencies; conflicts; conflict_class; depopts } =
    OpamFile.OPAM.empty
    |> OpamFile.OPAM.with_name (Opam_compatible_package_name.to_opam_package_name name)
    |> OpamFile.OPAM.with_depends
         (Package_dependency.list_to_opam_filtered_formula dependencies)
    |> OpamFile.OPAM.with_conflicts
         (Package_dependency.list_to_opam_filtered_formula conflicts)
    |> OpamFile.OPAM.with_conflict_class
         (List.map conflict_class ~f:Opam_compatible_package_name.to_opam_package_name)
    |> OpamFile.OPAM.with_depopts
         (Package_dependency.list_to_opam_filtered_formula depopts)
  ;;

  let opam_filtered_dependency_formula { dependencies; _ } =
    Package_dependency.list_to_opam_filtered_formula dependencies
  ;;

  let dependency_set { dependencies; _ } = Dependency_set.of_list dependencies
  let list_dependency_set ts = List.map ts ~f:dependency_set |> Dependency_set.union_all

  let list_non_local_dependency_set ts =
    List.fold_left ts ~init:(list_dependency_set ts) ~f:(fun acc { name; _ } ->
      Opam_compatible_package_name.Map.remove acc name)
  ;;
end

let for_solver
  { name; version = _; dependencies; conflicts; conflict_class; loc = _; depopts }
  =
  { For_solver.name; dependencies; conflicts; conflict_class; depopts }
;;
