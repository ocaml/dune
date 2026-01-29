open Import
open Dune_opam

let resolve_depopts ~resolve depopts =
  let rec collect acc depopts =
    match (depopts : OpamTypes.filtered_formula) with
    | Or ((Atom (_, _) as dep), depopts) -> collect (dep :: acc) depopts
    | Atom (_, _) as dep -> dep :: acc
    | Empty -> acc
    | _ ->
      (* We rely on depopts always being a list of or'ed package names. Opam
         verifies this for us at parsing time. Packages defined in dune-project
         files have this restriction for depopts and regular deps *)
      Code_error.raise "invalid depopts" [ "depopts", Opam_dyn.filtered_formula depopts ]
  in
  OpamFormula.ors_to_list depopts
  |> List.concat_map ~f:(fun x ->
    collect [] x
    |> List.rev
    |> List.concat_map ~f:(fun depopt ->
      match resolve depopt with
      | Error _ -> []
      | Ok { Resolve_opam_formula.post = _; regular } ->
        (* CR-someday rgrinberg: think about post deps *)
        regular))
;;

(* Translate the entire depexts field from the opam file into the lockfile by
   way of the slang dsl. Note that this preserves platform variables such as
   "os" and "os-distribution", which is different from how the "build",
   "install" and "depends" fields are treated, where platform variables are
   substituded with concrete values at solve time. There are many different
   Linux distributions and it's possible that some depexts will have different
   names on each distro and possibly also for different versions of the same
   distro. Users are not expected to solve their project for each
   distribution/version as that would take too long, instead opting to solve
   without a distro/version specified to create a package solution that's
   likely to work across all distros (except perhaps some unconventional
   distros such as Alpine). However even when using a general package solution,
   it's important that Dune is able to tell users the names of depexts tailored
   specifically for their current distro at build time. Thus, information
   mapping distro/version to package names must be preserved in lockfiles when
   solving. Opam allows depexts to be filtered by arbitrary filter expressions,
   which is why the slang dsl is needed as opposed to (say) a map from
   distro/version to depext name. *)
let depexts_to_conditional_external_dependencies package depexts =
  let open Result.O in
  List.map depexts ~f:(fun (sys_pkgs, filter) ->
    let external_package_names =
      OpamSysPkg.Set.to_list_map OpamSysPkg.to_string sys_pkgs
    in
    let+ condition =
      Filter.to_blang ~package ~loc:Loc.none filter >>| Slang.simplify_blang
    in
    let enabled_if =
      if Slang.Blang.equal condition Slang.Blang.true_
      then `Always
      else `Conditional condition
    in
    { Lock_dir.Depexts.external_package_names; enabled_if })
  |> Result.List.all
;;

let opam_package_to_lock_file_pkg
      solver_env
      stats_updater
      version_by_package_name
      opam_package
      ~pinned
      resolved_package
      ~portable_lock_dir
  =
  let open Result.O in
  let name = Package_name.of_opam_package_name (OpamPackage.name opam_package) in
  let version =
    OpamPackage.version opam_package |> Package_version.of_opam_package_version
  in
  let opam_file = Resolved_package.opam_file resolved_package in
  let loc = Resolved_package.loc resolved_package in
  let extra_sources =
    OpamFile.OPAM.extra_sources opam_file
    |> List.map ~f:(fun (opam_basename, opam_url) ->
      ( Path.Local.of_string (OpamFilename.Base.to_string opam_basename)
      , let url = Loc.none, OpamFile.URL.url opam_url in
        let checksum =
          match OpamFile.URL.checksum opam_url with
          | [] -> None
          | checksum :: _ -> Some (Loc.none, Checksum.of_opam_hash checksum)
        in
        { Source.url; checksum } ))
  in
  let info =
    let url = OpamFile.OPAM.url opam_file in
    let source =
      Option.map url ~f:(fun (url : OpamFile.URL.t) ->
        let checksum =
          OpamFile.URL.checksum url
          |> List.hd_opt
          |> Option.map ~f:(fun hash -> Loc.none, Checksum.of_opam_hash hash)
        in
        let url = Loc.none, OpamFile.URL.url url in
        { Source.url; checksum })
    in
    let dev =
      pinned
      ||
      match url with
      | None -> false
      | Some url -> List.is_empty (OpamFile.URL.checksum url)
    in
    let avoid = List.mem opam_file.flags Pkgflag_AvoidVersion ~equal:Poly.equal in
    { Lock_dir.Pkg_info.name; version; dev; avoid; source; extra_sources }
  in
  let depends =
    let resolve what =
      Resolve_opam_formula.filtered_formula_to_package_names
        ~with_test:false
        ~packages:version_by_package_name
        ~env:
          (Opam_env.add_self_to_filter_env
             opam_package
             (Solver_env.add_sentinel_values_for_unset_platform_vars solver_env
              |> Solver_env.to_env))
        what
    in
    let depends =
      match resolve opam_file.depends with
      | Ok { regular; _ } -> regular
      | Error (`Formula_could_not_be_satisfied hints) ->
        Code_error.raise
          "Dependencies of package can't be satisfied from packages in solution"
          [ "package", Dyn.string (opam_package |> OpamPackage.to_string)
          ; "hints", Dyn.list Resolve_opam_formula.Unsatisfied_formula_hint.to_dyn hints
          ]
    in
    let depopts =
      resolve_depopts ~resolve opam_file.depopts
      |> List.filter ~f:(fun package_name ->
        not (List.mem depends package_name ~equal:Package_name.equal))
    in
    depends @ depopts
    |> List.map ~f:(fun name -> { Lock_dir.Dependency.loc = Loc.none; name })
  in
  let build_env = Dune_opam.Package.wrap_build_env opam_file in
  let get_solver_var variable_name =
    Solver_stats.Updater.expand_variable stats_updater variable_name;
    Solver_env.get solver_env variable_name
  in
  let* build_command =
    if Resolved_package.dune_build resolved_package
    then Ok (Some Lock_dir.Build_command.Dune)
    else (
      let subst_step =
        OpamFile.OPAM.substs opam_file
        |> List.map ~f:(fun x ->
          let x = OpamFilename.Base.to_string x in
          let input = String_with_vars.make_text Loc.none (x ^ ".in") in
          let output = String_with_vars.make_text Loc.none x in
          Action.Substitute (input, output))
      in
      let+ patch_step =
        OpamFile.OPAM.patches opam_file
        |> List.map ~f:(fun (basename, filter) ->
          let action =
            Action.Patch
              (String_with_vars.make_text Loc.none (OpamFilename.Base.to_string basename))
          in
          match filter with
          | None -> Ok action
          | Some filter ->
            let+ blang =
              Filter.to_blang ~package:opam_package ~loc:Loc.none filter
              >>| Slang.simplify_blang
            in
            Action.When (blang, action))
        |> Result.List.all
      and+ build_step =
        Opam_command.to_actions
          get_solver_var
          loc
          opam_package
          (OpamFile.OPAM.build opam_file)
      in
      List.concat [ subst_step; patch_step; build_step ]
      |> Dune_opam.Package.make_action
      |> Option.map ~f:build_env
      |> Option.map ~f:(fun action -> Lock_dir.Build_command.Action action))
  in
  (* Some lockfile fields contain a choice of values predicated on a set of
     platform variables to allow lockfiles to be portable across different
     platforms. Each invocation of the solver produces a solution associated
     with a single set of platform variables (those in [solver_env]).
     [lockfile_field_choice value] creates a choice with a single possible
     value predicated by the platform variables in [solver_env]. The
     solver may be run multiple times, and the choice fields of lockfiles
     will be merged such that different values can be chosen on different
     platforms. *)
  let lockfile_field_choice value =
    Lock_dir.Conditional_choice.singleton solver_env value
  in
  let build_command =
    Option.map build_command ~f:lockfile_field_choice
    |> Option.value ~default:Lock_dir.Conditional_choice.empty
  in
  let* depexts =
    if portable_lock_dir
    then
      depexts_to_conditional_external_dependencies
        opam_package
        (OpamFile.OPAM.depexts opam_file)
    else (
      (* In the non-portable case, only include depexts for the current platform. *)
      let external_package_names =
        OpamFile.OPAM.depexts opam_file
        |> List.concat_map ~f:(fun (sys_pkgs, filter) ->
          let env = Solver_env.to_env solver_env in
          if OpamFilter.eval_to_bool ~default:false env filter
          then OpamSysPkg.Set.to_list_map OpamSysPkg.to_string sys_pkgs
          else [])
      in
      let depexts =
        if List.is_empty external_package_names
        then []
        else [ { Lock_dir.Depexts.external_package_names; enabled_if = `Always } ]
      in
      Ok depexts)
  in
  let+ install_command =
    OpamFile.OPAM.install opam_file
    |> Opam_command.to_actions get_solver_var loc opam_package
    >>| Dune_opam.Package.make_action
    >>| Option.map ~f:(fun action -> lockfile_field_choice (build_env action))
    >>| Option.value ~default:Lock_dir.Conditional_choice.empty
  in
  let exported_env =
    OpamFile.OPAM.env opam_file |> List.map ~f:Opam_env.opam_env_update_to_env_update
  in
  let depends = lockfile_field_choice depends in
  let enabled_on_platforms =
    [ Solver_env.remove_all_except_platform_specific solver_env ]
  in
  { Lock_dir.Pkg.build_command
  ; install_command
  ; depends
  ; depexts
  ; info
  ; exported_env
  ; enabled_on_platforms
  }
;;
