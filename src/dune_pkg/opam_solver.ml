open Stdune
module Package_name = Dune_lang.Package_name

module type CONTEXT = Opam_0install.S.CONTEXT

(* Helper module for working with [OpamTypes.filter] *)
module Filter : sig
  type filter := OpamTypes.filter

  (** Replace all flags with their values. Variables that aren't defined in
      [Solver_env.Flag] are left unresolved. *)
  val resolve_flags : Solver_env.Flag.Set.t -> filter -> filter

  (** Perform two transformations on a [filter]:

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
  val resolve_sys_var_treating_unset_vars_as_wildcards :
    Solver_env.Sys_var.Bindings.t -> filter -> filter

  (** Replace all variables from a [Solver_env.t] with their values treating
      unset system environment variables as wildcards. See the documentation of
      [resolve_sys_var_treating_unset_vars_as_wildcards] for more information. *)
  val resolve_solver_env_treating_unset_sys_vars_as_wildcards :
    Solver_env.t -> filter -> filter

  val eval_to_bool : filter -> (bool, [ `Not_a_bool of string ]) result
end = struct
  open OpamTypes

  let resolve_flags flags =
    OpamFilter.map_up (function
      | FIdent ([], variable, None) as filter -> (
        match
          Solver_env.Flag.of_string_opt (OpamVariable.to_string variable)
        with
        | Some flag -> FBool (Solver_env.Flag.Set.mem flags flag)
        | None -> filter)
      | other -> other)

  (* Returns true iff a variable is an opam system environment variable *)
  let is_variable_sys variable =
    Solver_env.Sys_var.of_string_opt (OpamVariable.to_string variable)
    |> Option.is_some

  let resolve_sys_var_treating_unset_vars_as_wildcards sys_var_bindings =
    OpamFilter.map_up (function
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
           [OpamFilter.map_up] traverses the formula bottom up, so variables
           with values in [sys_var_bindings] will have been substituted for
           those values already by the time control gets here.*)
        if is_variable_sys variable then FBool true else filter
      | other -> other)

  let resolve_solver_env_treating_unset_sys_vars_as_wildcards
      { Solver_env.flags; sys } filter =
    filter |> resolve_flags flags
    |> resolve_sys_var_treating_unset_vars_as_wildcards sys

  let eval_to_bool filter =
    try Ok (OpamFilter.eval_to_bool ~default:false (Fun.const None) filter)
    with Invalid_argument msg -> Error (`Not_a_bool msg)
end

(* Helper module for working with [OpamTypes.filtered_formula] *)
module Filtered_formula : sig
  open OpamTypes

  (** Transform the filter applied to each formula according to a function [g] *)
  val map_filters : f:(filter -> filter) -> filtered_formula -> filtered_formula
end = struct
  open OpamTypes

  let map_filters ~f =
    OpamFilter.gen_filter_formula
      (OpamFormula.partial_eval (function
        | Filter flt -> `Formula (Atom (Filter (f flt)))
        | Constraint _ as constraint_ -> `Formula (Atom constraint_)))
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

  type rejection = Unavailable

  let pp_rejection f = function
    | Unavailable -> Fmt.string f "Availability condition not satisfied"

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

  let is_opam_available =
    (* The solver can call this function several times on the same package. If
       the package contains an invalid `available` filter we want to print a
       warning, but only once per package. This variable will keep track of the
       packages for which we've printed a warning. *)
    let warned_packages = ref OpamPackage.Set.empty in
    fun t opam ->
      let available = OpamFile.OPAM.available opam in
      let available_vars_resolved =
        Filter.resolve_solver_env_treating_unset_sys_vars_as_wildcards
          t.solver_env available
      in
      match Filter.eval_to_bool available_vars_resolved with
      | Ok available -> available
      | Error error ->
        let package = OpamFile.OPAM.package opam in
        if not (OpamPackage.Set.mem package !warned_packages) then (
          warned_packages := OpamPackage.Set.add package !warned_packages;
          let package_string =
            OpamFile.OPAM.package opam |> OpamPackage.to_string
          in
          let available_string = OpamFilter.to_string available in
          match error with
          | `Not_a_bool msg ->
            User_warning.emit
              [ Pp.textf
                  "Ignoring package %s as its `available` filter can't be \
                   resolved to a boolean value."
                  package_string
              ; Pp.textf "available: %s" available_string
              ; Pp.text msg
              ]);
        false

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
            let opam_file_result =
              if is_opam_available t opam_file then Ok opam_file
              else Error Unavailable
            in
            (OpamFile.OPAM.version opam_file, opam_file_result)))

  let user_restrictions _ _ = None

  let filter_deps t package filtered_formula =
    let package_is_local =
      OpamPackage.Name.Map.mem (OpamPackage.name package) t.local_packages
    in
    (if package_is_local then
       Filtered_formula.map_filters filtered_formula
         ~f:(Filter.resolve_flags t.solver_env.flags)
     else filtered_formula)
    |> Filtered_formula.map_filters
         ~f:
           (Filter.resolve_sys_var_treating_unset_vars_as_wildcards
              t.solver_env.sys)
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
