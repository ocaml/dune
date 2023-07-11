open Stdune
module Package_name = Dune_lang.Package_name

module type CONTEXT = Opam_0install.S.CONTEXT

(* Helper module for working with [OpamTypes.filtered_formula] *)
module Filtered_formula : sig
  open OpamTypes

  (** Update a filtered formula setting the "with-test" and "with-doc" variables
      to true. *)
  val with_flags : Solver_env.Flag.Set.t -> filtered_formula -> filtered_formula

  (** Perform two transformations on a [filtered_formula]:

      - Substitute system environment variables with their values
      - Resolves comparisons with unset system environment variables to true

      This creates a formula which is as permissive as possible within the
      constraints for system environment variables specified by the user. The
      intention is to generate lockdirs which will work on as many systems as
      possible, but to allow the user to constrain this by setting environment
      variables to handle situations where the most permissive solve is not
      possible or otherwise produces undesirable outcomes (e.g. when there are
      mutually-incompatible os-specific packages for different operating
      systems). *)
  val resolve_sys_env_variables :
    Solver_env.Sys_var.Bindings.t -> filtered_formula -> filtered_formula
end = struct
  open OpamTypes

  (* [map_formula_filters_bottom_up t ~f] calls [f] on each node of each filter
     in t, bottom-up *)
  let map_filters_bottom_up ~f =
    OpamFilter.gen_filter_formula
      (OpamFormula.partial_eval (function
        | Filter flt -> `Formula (Atom (Filter (OpamFilter.map_up f flt)))
        | Constraint _ as constraint_ -> `Formula (Atom constraint_)))

  (* update a filtered formula replacing instances of a given variable with a given boolean value *)
  let resolve_simple_bool_variable filtered_formula variable_to_resolve value =
    map_filters_bottom_up filtered_formula ~f:(function
      | FIdent ([], variable, None) as filter ->
        if OpamVariable.equal variable variable_to_resolve then FBool value
        else filter
      | other -> other)

  let with_flags flags filtered_formula =
    Solver_env.Flag.Set.fold flags ~init:filtered_formula ~f:(fun flag acc ->
        resolve_simple_bool_variable acc
          (OpamVariable.of_string (Solver_env.Flag.to_string flag))
          true)

  let is_variable_sys variable =
    Solver_env.Sys_var.of_string_opt (OpamVariable.to_string variable)
    |> Option.is_some

  let resolve_sys_env_variables sys_var_bindings =
    map_filters_bottom_up ~f:(function
      | FIdent ([], variable, None) as filter -> (
        (* System environment variables which are set in the solver environment
           are substituted for their values here. *)
        let sys_var_value_opt =
          Solver_env.Sys_var.of_string_opt (OpamVariable.to_string variable)
          |> Option.bind ~f:(Solver_env.Sys_var.Bindings.get sys_var_bindings)
        in
        match sys_var_value_opt with
        | None -> filter
        | Some sys_var_value -> FString sys_var_value)
      | ( FOp (FIdent (_, variable, _), _, _)
        | FOp (_, _, FIdent (_, variable, _)) ) as filter ->
        (* Comparisons with unset system environment variables resolve to
           true. This is so that we add dependencies guarded by filters on
           unset system variables. For example if a package has a
           linux-only and a macos-only dependency and the user hasn't
           specified that they only want to solve for a specific os, then
           we should add both the linux-only and macos-only dependencies
           to the solution.

           Note that this branch is only followed for unset variables as
           [map_filters_bottom_up] traverses the formula bottom up, so
           variables with values in [solver_env] will have been substituted for
           those values already by the time control gets here.*)
        if is_variable_sys variable then FBool true else filter
      | other -> other)
end

module Context_for_dune = struct
  let local_package_default_version =
    OpamPackage.Version.of_string Lock_dir.Pkg_info.default_version

  type t =
    { repo : Opam_repo.t
    ; version_preference : Version_preference.t
    ; local_packages : OpamFile.OPAM.t OpamPackage.Name.Map.t
    ; solver_env : Solver_env.t
    }

  let create ~solver_env ~repo ~local_packages ~version_preference =
    { repo; version_preference; local_packages; solver_env }

  type rejection = |

  let pp_rejection = Fmt.nop

  let opam_version_compare t =
    let opam_package_version_compare a b =
      OpamPackage.Version.compare a b |> Ordering.of_int
    in
    let opam_file_compare_by_version a b =
      opam_package_version_compare (OpamFile.OPAM.version a)
        (OpamFile.OPAM.version b)
    in
    match t.version_preference with
    | Oldest -> opam_file_compare_by_version
    | Newest -> Ordering.reverse opam_file_compare_by_version

  let candidates t name =
    match OpamPackage.Name.Map.find_opt name t.local_packages with
    | Some opam_file ->
      let version =
        Option.value opam_file.version ~default:local_package_default_version
      in
      [ (version, Ok opam_file) ]
    | None -> (
      match Opam_repo.load_all_versions t.repo name with
      | Error `Package_not_found ->
        (* The CONTEXT interface doesn't give us a way to report this type of
           error and there's not enough context to give a helpful error message
           so just tell opam_0install that there are no versions of this
           package available (technically true) and let it produce the error
           message. *)
        []
      | Ok opam_files ->
        let opam_files_in_priority_order =
          List.sort opam_files ~compare:(opam_version_compare t)
        in
        List.map opam_files_in_priority_order ~f:(fun opam_file ->
            (OpamFile.OPAM.version opam_file, Ok opam_file)))

  let user_restrictions _ _ = None

  (* Override [filter_deps] so that local packages get test and doc dependencies *)
  let filter_deps t package filtered_formula =
    let package_is_local =
      OpamPackage.Name.Map.mem (OpamPackage.name package) t.local_packages
    in
    (if package_is_local then
       Filtered_formula.with_flags t.solver_env.flags filtered_formula
     else filtered_formula)
    |> Filtered_formula.resolve_sys_env_variables t.solver_env.sys
    |> OpamFilter.filter_deps ~build:true ~post:true ~dev:false ~default:false
         ~test:false ~doc:false
end

module Solver = Opam_0install.Solver.Make (Context_for_dune)

module Summary = struct
  type t = { opam_packages_to_lock : OpamPackage.t list }

  let selected_packages_message t ~lock_dir_path =
    let parts =
      match t.opam_packages_to_lock with
      | [] ->
        [ Pp.tag User_message.Style.Success
            (Pp.text "(no dependencies to lock)")
        ]
      | opam_packages_to_lock ->
        List.map opam_packages_to_lock ~f:(fun package ->
            Pp.text (OpamPackage.to_string package))
    in
    User_message.make
      (Pp.textf "Solution for %s:"
         (Path.Source.to_string_maybe_quoted lock_dir_path)
      :: parts)
end

let opam_package_to_lock_file_pkg ~repo ~local_packages opam_package =
  let name = OpamPackage.name opam_package in
  let version =
    OpamPackage.version opam_package |> OpamPackage.Version.to_string
  in
  let dev = OpamPackage.Name.Map.mem name local_packages in
  let info =
    { Lock_dir.Pkg_info.name =
        Package_name.of_string (OpamPackage.Name.to_string name)
    ; version
    ; dev
    ; source = None
    ; extra_sources = []
    }
  in
  let opam_file =
    match OpamPackage.Name.Map.find_opt name local_packages with
    | None -> Opam_repo.load_opam_package repo opam_package
    | Some local_package -> local_package
  in
  (* This will collect all the atoms from the package's dependency formula regardless of conditions *)
  let deps =
    OpamFormula.fold_right
      (fun acc (name, _condition) -> name :: acc)
      [] opam_file.depends
    |> List.map ~f:(fun name ->
           (Loc.none, Package_name.of_string (OpamPackage.Name.to_string name)))
  in
  { Lock_dir.Pkg.build_command = None
  ; install_command = None
  ; deps
  ; info
  ; exported_env = []
  }

let solve_package_list local_packages context =
  let result =
    try
      (* [Solver.solve] returns [Error] when it's unable to find a solution to
         the dependencies, but can also raise exceptions, for example if opam
         is unable to parse an opam file in the package repository. To prevent
         an unexpected opam exception from crashing dune, we catch all
         exceptions raised by the solver and report them as [User_error]s
         instead. *)
      Solver.solve context (OpamPackage.Name.Map.keys local_packages)
    with
    | OpamPp.(Bad_format _ | Bad_format_list _ | Bad_version _) as bad_format ->
      User_error.raise [ Pp.text (OpamPp.string_of_bad_format bad_format) ]
    | unexpected_exn ->
      Code_error.raise "Unexpected exception raised while solving dependencies"
        [ ("exception", Exn.to_dyn unexpected_exn) ]
  in
  match result with
  | Error e -> Error (`Diagnostic_message (Solver.diagnostics e |> Pp.text))
  | Ok packages -> Ok (Solver.packages_of_result packages)

let solve_lock_dir solver_env version_preference repo ~local_packages =
  let is_local_package package =
    OpamPackage.Name.Map.mem (OpamPackage.name package) local_packages
  in
  let context =
    Context_for_dune.create ~solver_env ~repo ~version_preference
      ~local_packages
  in
  solve_package_list local_packages context
  |> Result.map ~f:(fun solution ->
         (* don't include local packages in the lock dir *)
         let opam_packages_to_lock =
           List.filter solution ~f:(Fun.negate is_local_package)
         in
         let summary = { Summary.opam_packages_to_lock } in
         let lock_dir =
           match
             Package_name.Map.of_list_map opam_packages_to_lock
               ~f:(fun opam_package ->
                 let pkg =
                   opam_package_to_lock_file_pkg ~repo ~local_packages
                     opam_package
                 in
                 (pkg.info.name, pkg))
           with
           | Error (name, _pkg1, _pkg2) ->
             Code_error.raise
               (sprintf "Solver selected multiple packages named \"%s\""
                  (Package_name.to_string name))
               []
           | Ok pkgs_by_name ->
             Lock_dir.create_latest_version pkgs_by_name ~ocaml:None
         in
         (summary, lock_dir))
