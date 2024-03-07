open! Import
module Package_constraint = Dune_lang.Package_constraint
module Digest = Dune_digest

type pin =
  { loc : Loc.t
  ; version : Package_version.t
  ; url : Loc.t * OpamUrl.t
  ; name : Package_name.t
  ; origin : [ `Dune | `Opam ]
  }

type pins = pin Package_name.Map.t

type t =
  { name : Package_name.t
  ; version : Package_version.t option
  ; dependencies : Package_dependency.t list
  ; conflicts : Package_dependency.t list
  ; conflict_class : Package_name.t list
  ; depopts : Package_dependency.t list
  ; pins : pins
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
  type t = Package_constraint.Set.t Package_name.Map.t

  let empty = Package_name.Map.empty

  let of_list =
    List.fold_left ~init:empty ~f:(fun acc { Package_dependency.name; constraint_ } ->
      Package_name.Map.update acc name ~f:(fun existing ->
        match existing, constraint_ with
        | None, None -> Some Package_constraint.Set.empty
        | None, Some constraint_ -> Some (Package_constraint.Set.singleton constraint_)
        | Some existing, None -> Some existing
        | Some existing, Some constraint_ ->
          Some (Package_constraint.Set.add existing constraint_)))
  ;;

  let union =
    Package_name.Map.union ~f:(fun _name a b -> Some (Package_constraint.Set.union a b))
  ;;

  let union_all = List.fold_left ~init:empty ~f:union

  let package_dependencies =
    Package_name.Map.to_list_map ~f:(fun name constraints ->
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
    if Package_name.Map.is_empty t
    then None
    else Some (encode_for_hash t |> Dune_sexp.to_string |> Dune_digest.string)
  ;;
end

module For_solver = struct
  type t =
    { name : Package_name.t
    ; dependencies : Package_dependency.t list
    ; conflicts : Package_dependency.t list
    ; depopts : Package_dependency.t list
    ; conflict_class : Package_name.t list
    ; pins : pins
    }

  let to_opam_file { name; dependencies; conflicts; conflict_class; depopts; pins = _ } =
    (* CR-rgrinberg: it's OK to ignore pins here since the solver doesn't touch
       them *)
    OpamFile.OPAM.empty
    |> OpamFile.OPAM.with_name (Package_name.to_opam_package_name name)
    |> OpamFile.OPAM.with_depends
         (Package_dependency.list_to_opam_filtered_formula dependencies)
    |> OpamFile.OPAM.with_conflicts
         (Package_dependency.list_to_opam_filtered_formula conflicts)
    |> OpamFile.OPAM.with_conflict_class
         (List.map conflict_class ~f:Package_name.to_opam_package_name)
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
      Package_name.Map.remove acc name)
  ;;
end

let for_solver
  { name; version = _; dependencies; conflicts; conflict_class; loc = _; depopts; pins }
  =
  { For_solver.name; dependencies; conflicts; conflict_class; depopts; pins }
;;

let of_package (t : Dune_lang.Package.t) =
  let module Package = Dune_lang.Package in
  let loc = Package.loc t in
  let version = Package.version t in
  let name = Package.name t in
  match Package.original_opam_file t with
  | None ->
    { name
    ; version
    ; dependencies = Package.depends t
    ; conflicts = Package.conflicts t
    ; depopts = Package.depopts t
    ; loc
    ; conflict_class = []
    ; pins = Package_name.Map.empty
    }
  | Some { file; contents = opam_file_string } ->
    let opam_file =
      Opam_file.read_from_string_exn ~contents:opam_file_string (Path.source file)
    in
    let convert_filtered_formula filtered_formula =
      Package_dependency.list_of_opam_filtered_formula loc filtered_formula
    in
    let dependencies = convert_filtered_formula (OpamFile.OPAM.depends opam_file) in
    let conflicts = convert_filtered_formula (OpamFile.OPAM.conflicts opam_file) in
    let depopts = convert_filtered_formula (OpamFile.OPAM.depopts opam_file) in
    let conflict_class =
      OpamFile.OPAM.conflict_class opam_file
      |> List.map ~f:Package_name.of_opam_package_name
    in
    let pins =
      match
        OpamFile.OPAM.pin_depends opam_file
        |> List.map ~f:(fun (pkg, url) ->
          let name = Package_name.of_opam_package_name (OpamPackage.name pkg) in
          let version =
            Package_version.of_opam_package_version (OpamPackage.version pkg)
          in
          let loc = Loc.in_file (Path.source file) in
          name, { loc; version; url = loc, url; name; origin = `Opam })
        |> Package_name.Map.of_list
      with
      | Ok x -> x
      | Error (_, pkg, _) ->
        User_error.raise
          ~loc:pkg.loc
          [ Pp.textf "package %s is already pinned" (Package_name.to_string pkg.name) ]
    in
    { name; version; dependencies; conflicts; depopts; loc; conflict_class; pins }
;;
