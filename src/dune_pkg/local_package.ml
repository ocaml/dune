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
  ; dependencies : OpamTypes.filtered_formula
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

module For_solver = struct
  type t =
    { name : Package_name.t
    ; dependencies : OpamTypes.filtered_formula
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
    |> OpamFile.OPAM.with_depends dependencies
    |> OpamFile.OPAM.with_conflicts
         (Package_dependency.list_to_opam_filtered_formula conflicts)
    |> OpamFile.OPAM.with_conflict_class
         (List.map conflict_class ~f:Package_name.to_opam_package_name)
    |> OpamFile.OPAM.with_depopts
         (Package_dependency.list_to_opam_filtered_formula depopts)
  ;;

  let opam_filtered_dependency_formula { dependencies; _ } = dependencies

  module T = struct
    type t = Package_name.t * OpamTypes.condition

    let to_dyn = Dyn.pair Package_name.to_dyn Opam_dyn.condition

    let compare (name, condition) (other_name, other_condition) =
      let open Ordering.O in
      let= () = Package_name.compare name other_name in
      let compare_filters filter filter' =
        let get_vars = function
          | OpamTypes.Constraint _ -> []
          | Filter filter -> Stdlib.List.sort Stdlib.compare (OpamFilter.variables filter)
        in
        match get_vars filter, get_vars filter' with
        | v :: _, v' :: _ -> Stdlib.compare v v'
        | [], _ :: _ -> 1
        | _ :: _, [] -> -1
        | [], [] -> 0
      in
      Ordering.of_int
        (OpamFormula.compare_formula compare_filters condition other_condition)
    ;;
  end

  module Map = Map.Make (T)
  module Set = Set.Make (T) (Map)

  let maximum_package_set formula =
    let rec loop = function
      | OpamFormula.Empty -> Set.empty
      | Atom (name, condition) ->
        let name = name |> OpamPackage.Name.to_string |> Package_name.of_string in
        Set.singleton (name, condition)
      | Block b -> loop b
      | And (l, r) | Or (l, r) ->
        let l = loop l in
        let r = loop r in
        Set.union l r
    in
    loop formula
  ;;

  let non_local_deps local_deps =
    let local_deps_names = Package_name.Set.of_list_map ~f:(fun d -> d.name) local_deps in
    List.map ~f:(fun local_dep -> maximum_package_set local_dep.dependencies) local_deps
    |> Set.union_all
    |> Set.filter ~f:(fun (name, _condition) ->
      (* remove all packages that are local deps, regardless of conditions *)
      not (Package_name.Set.mem local_deps_names name))
  ;;

  let non_local_dependency_hash local_deps =
    let non_local_deps = non_local_deps local_deps in
    match Set.is_empty non_local_deps with
    | true -> None
    | false ->
      let hashable =
        non_local_deps
        |> Set.to_list
        |> List.map ~f:(fun (name, condition) ->
          let condition = condition |> Opam_dyn.condition |> Dyn.to_string in
          let name = Package_name.to_string name in
          sprintf "%s %s" name condition)
        |> String.concat ~sep:"\n"
      in
      Some (Dependency_hash.string hashable)
  ;;

  let any_non_local_dependency_name local_deps =
    non_local_deps local_deps |> Set.choose_exn |> fst
  ;;

  let dependency_names t =
    maximum_package_set t.dependencies |> Set.to_list |> List.map ~f:fst
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
    let dependencies =
      t |> Package.depends |> Package_dependency.list_to_opam_filtered_formula
    in
    { name
    ; version
    ; dependencies
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
    let convert_filtered_formula = Package_dependency.list_of_opam_filtered_formula loc in
    let dependencies = OpamFile.OPAM.depends opam_file in
    let conflicts = convert_filtered_formula `And (OpamFile.OPAM.conflicts opam_file) in
    let depopts = convert_filtered_formula `Or (OpamFile.OPAM.depopts opam_file) in
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
