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

type command_source =
  | Assume_defaults
  | Opam_file of
      { build : OpamTypes.command list
      ; install : OpamTypes.command list
      }

type t =
  { name : Package_name.t
  ; version : Package_version.t option
  ; dependencies : Dependency_formula.t
  ; conflicts : Package_dependency.t list
  ; conflict_class : Package_name.t list
  ; depopts : Package_dependency.t list
  ; pins : pins
  ; loc : Loc.t
  ; command_source : command_source
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

  let of_dependency_formula formula =
    match Dependency_formula.has_entries formula with
    | false -> None
    | true ->
      let hashable = formula |> Dependency_formula.to_dyn |> Dyn.to_string in
      Some (string hashable)
  ;;
end

module For_solver = struct
  type t =
    { name : Package_name.t
    ; dependencies : Dependency_formula.t
    ; conflicts : Package_dependency.t list
    ; depopts : Package_dependency.t list
    ; conflict_class : Package_name.t list
    ; pins : pins
    ; build : OpamTypes.command list
    ; install : OpamTypes.command list
    }

  let to_opam_file
        { name
        ; dependencies
        ; conflicts
        ; conflict_class
        ; depopts
        ; pins = _
        ; build
        ; install
        }
    =
    (* CR-rgrinberg: it's OK to ignore pins here since the solver doesn't touch
       them *)
    OpamFile.OPAM.empty
    |> OpamFile.OPAM.with_name (Package_name.to_opam_package_name name)
    |> OpamFile.OPAM.with_depends (Dependency_formula.to_filtered_formula dependencies)
    |> OpamFile.OPAM.with_conflicts
         (List.map conflicts ~f:Package_dependency.to_opam_filtered_formula
          |> OpamFormula.ors)
    |> OpamFile.OPAM.with_conflict_class
         (List.map conflict_class ~f:Package_name.to_opam_package_name)
    |> OpamFile.OPAM.with_depopts
         (List.map depopts ~f:Package_dependency.to_opam_filtered_formula
          |> OpamFormula.ands)
    |> OpamFile.OPAM.with_install install
    |> OpamFile.OPAM.with_build build
  ;;

  let non_local_dependencies local_deps =
    let local_deps_names = Package_name.Set.of_list_map ~f:(fun d -> d.name) local_deps in
    let formula =
      List.map ~f:(fun { dependencies; _ } -> dependencies) local_deps
      |> Dependency_formula.ands
    in
    Dependency_formula.remove_packages formula local_deps_names
  ;;
end

let for_solver
      { name
      ; version = _
      ; dependencies
      ; conflicts
      ; conflict_class
      ; loc = _
      ; depopts
      ; pins
      ; command_source
      }
  =
  let build, install =
    match command_source with
    | Assume_defaults -> [], []
    | Opam_file { build; install } -> build, install
  in
  { For_solver.name
  ; dependencies
  ; conflicts
  ; conflict_class
  ; depopts
  ; pins
  ; build
  ; install
  }
;;

let of_package (t : Dune_lang.Package.t) =
  let module Package = Dune_lang.Package in
  let loc = Package.loc t in
  let version = Package.version t in
  let name = Package.name t in
  Printf.eprintf "Attempting to load of_package: %s\n" (Package.Name.to_string name);
  match (Package.dune_package t, Package.opam_package t) with
  | None, None -> assert false
  | Some _, Some _ -> assert false
  | Some t, None ->
    Printf.eprintf "No original opam file\n";
    let dependencies = t |> Dune_lang.Dune_package.depends |> Dependency_formula.of_dependencies in
    { name
    ; version
    ; dependencies
    ; conflicts = Dune_lang.Dune_package.conflicts t
    ; depopts = Dune_lang.Dune_package.depopts t
    ; loc
    ; conflict_class = []
    ; pins = Package_name.Map.empty
    ; command_source = Assume_defaults
    }
  | None, Some o ->
    Printf.eprintf "Yes original opam file\n";
    let command_source =
      (* Opam_file *)
      (*   { build = opam_file |> OpamFile.OPAM.build *)
      (*   ; install = opam_file |> OpamFile.OPAM.install *)
      (*   } *)
      Assume_defaults
    in
    let dependencies =
      o |> Dune_lang.Package.Opam_package.depends |> Dependency_formula.of_filtered_formula
    in
    let conflicts =
      (* OpamFile.OPAM.conflicts opam_file |> Package_dependency.list_of_opam_disjunction loc *)
      []
    in
    let depopts =
      (* OpamFile.OPAM.depopts opam_file |> Package_dependency.list_of_opam_disjunction loc *)
      []
    in
    let conflict_class =
      (* OpamFile.OPAM.conflict_class opam_file *)
      (* |> List.map ~f:Package_name.of_opam_package_name *)
      []
    in
    let pins =
      (* match *)
      (*   OpamFile.OPAM.pin_depends opam_file *)
      (*   |> List.map ~f:(fun (pkg, url) -> *)
      (*     let name = Package_name.of_opam_package_name (OpamPackage.name pkg) in *)
      (*     let version = *)
      (*       Package_version.of_opam_package_version (OpamPackage.version pkg) *)
      (*     in *)
      (*     let loc = Loc.in_file (Path.source file) in *)
      (*     name, { loc; version; url = loc, url; name; origin = `Opam }) *)
      (*   |> Package_name.Map.of_list *)
      (* with *)
      (* | Ok x -> x *)
      (* | Error (_, pkg, _) -> *)
      (*   User_error.raise *)
      (*     ~loc:pkg.loc *)
      (*     [ Pp.textf "package %s is already pinned" (Package_name.to_string pkg.name) ] *)
      Package_name.Map.empty
    in
    { name
    ; version
    ; dependencies
    ; conflicts
    ; depopts
    ; loc
    ; conflict_class
    ; pins
    ; command_source
    }
;;
