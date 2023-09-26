open Import

module type CONTEXT = Opam_0install.S.CONTEXT

(* Helper module for working with [OpamTypes.filter] *)
module Filter : sig
  type filter := OpamTypes.filter

  (** Substitute variables with their values.

      Comparisons with unset system environment variables resolve to true,
      treating them as wildcards. This creates a formula which is as permissive
      as possible within the constraints for system environment variables
      specified by the user. The intention is to generate lockdirs which will
      work on as many systems as possible, but to allow the user to constrain
      this by setting environment variables to handle situations where the most
      permissive solve is not possible or otherwise produces undesirable
      outcomes (e.g. when there are mutually-incompatible os-specific packages
      for different operating systems). *)
  val resolve_solver_env_treating_unset_sys_variables_as_wildcards
    :  Solver_env.t
    -> filter
    -> filter

  val eval_to_bool : filter -> (bool, [ `Not_a_bool of string ]) result
end = struct
  open OpamTypes

  (* Returns true iff a variable is an opam system environment variable *)
  let is_variable_sys variable =
    Solver_env.Variable.Sys.of_string_opt (OpamVariable.to_string variable)
    |> Option.is_some
  ;;

  let resolve_solver_env_treating_unset_sys_variables_as_wildcards solver_env =
    OpamFilter.map_up (function
      | FIdent ([], variable, None) as filter ->
        (match Solver_env.Variable.of_string_opt (OpamVariable.to_string variable) with
         | None -> filter
         | Some variable ->
           (match Solver_env.get solver_env variable with
            | Unset_sys -> filter
            | String string -> FString string
            | Bool bool -> FBool bool))
      | (FOp (FIdent (_, variable, _), _, _) | FOp (_, _, FIdent (_, variable, _))) as
        filter ->
        if is_variable_sys variable
        then
          (* Comparisons with unset system environment variables resolve to
             true. This is so that we add dependencies guarded by filters on
             unset system variables. For example if a package has a linux-only
             and a macos-only dependency and the user hasn't specified that they
             only want to solve for a specific os, then we should add both the
             linux-only and macos-only dependencies to the solution.

             Note that this branch is only followed for unset variables as
             [OpamFilter.map_up] traverses the formula bottom up, so variables
             with values in [solver_env] will have been substituted for those
             values already by the time control gets here.*)
          FBool true
        else filter
      | other -> other)
  ;;

  let eval_to_bool filter =
    try Ok (OpamFilter.eval_to_bool ~default:false (Fun.const None) filter) with
    | Invalid_argument msg -> Error (`Not_a_bool msg)
  ;;
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
  ;;
end

module Context_for_dune = struct
  let local_package_default_version =
    OpamPackage.Version.of_string Lock_dir.Pkg_info.default_version
  ;;

  type t =
    { repos : Opam_repo.t list
    ; version_preference : Version_preference.t
    ; local_packages : OpamFile.OPAM.t OpamPackage.Name.Map.t
    ; solver_env : Solver_env.t
    }

  let create ~solver_env ~repos ~local_packages ~version_preference =
    { repos; version_preference; local_packages; solver_env }
  ;;

  type rejection = Unavailable

  let pp_rejection f = function
    | Unavailable -> Fmt.string f "Availability condition not satisfied"
  ;;

  let opam_version_compare t =
    let opam_package_version_compare a b =
      OpamPackage.Version.compare a b |> Ordering.of_int
    in
    let opam_file_compare_by_version a b =
      opam_package_version_compare (OpamFile.OPAM.version a) (OpamFile.OPAM.version b)
    in
    match t.version_preference with
    | Oldest -> opam_file_compare_by_version
    | Newest -> Ordering.reverse opam_file_compare_by_version
  ;;

  let is_opam_available =
    (* The solver can call this function several times on the same package. If
       the package contains an invalid `available` filter we want to print a
       warning, but only once per package. This variable will keep track of the
       packages for which we've printed a warning. *)
    let warned_packages = ref OpamPackage.Set.empty in
    fun t opam ->
      let available = OpamFile.OPAM.available opam in
      let available_vars_resolved =
        Filter.resolve_solver_env_treating_unset_sys_variables_as_wildcards
          t.solver_env
          available
      in
      match Filter.eval_to_bool available_vars_resolved with
      | Ok available -> available
      | Error error ->
        let package = OpamFile.OPAM.package opam in
        if not (OpamPackage.Set.mem package !warned_packages)
        then (
          warned_packages := OpamPackage.Set.add package !warned_packages;
          let package_string = OpamFile.OPAM.package opam |> OpamPackage.to_string in
          let available_string = OpamFilter.to_string available in
          match error with
          | `Not_a_bool msg ->
            User_warning.emit
              [ Pp.textf
                  "Ignoring package %s as its `available` filter can't be resolved to a \
                   boolean value."
                  package_string
              ; Pp.textf "available: %s" available_string
              ; Pp.text msg
              ]);
        false
  ;;

  let candidates t name =
    match OpamPackage.Name.Map.find_opt name t.local_packages with
    | Some opam_file ->
      let version =
        Option.value opam_file.version ~default:local_package_default_version
      in
      [ version, Ok opam_file ]
    | None ->
      (match Opam_repo.load_all_versions t.repos name with
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
             if is_opam_available t opam_file then Ok opam_file else Error Unavailable
           in
           OpamFile.OPAM.version opam_file, opam_file_result))
  ;;

  let user_restrictions _ _ = None

  let filter_deps t package filtered_formula =
    let package_is_local =
      OpamPackage.Name.Map.mem (OpamPackage.name package) t.local_packages
    in
    let solver_env =
      if package_is_local
      then t.solver_env
      else
        (* Flag variables pertain only to local packages. This is because these
           variables enable dependencies on test and documentation packages and
           we don't want to pull in test and doc dependencies for dependencies
           of local packages. *)
        Solver_env.clear_flags t.solver_env
    in
    Filtered_formula.map_filters
      filtered_formula
      ~f:(Filter.resolve_solver_env_treating_unset_sys_variables_as_wildcards solver_env)
    |> OpamFilter.filter_deps
         ~build:true
         ~post:true
         ~dev:false
         ~default:false
         ~test:false
         ~doc:false
  ;;
end

module Solver = Opam_0install.Solver.Make (Context_for_dune)

module Summary = struct
  type t = { opam_packages_to_lock : OpamPackage.t list }

  let selected_packages_message t ~lock_dir_path =
    let parts =
      match t.opam_packages_to_lock with
      | [] -> [ Pp.tag User_message.Style.Success (Pp.text "(no dependencies to lock)") ]
      | opam_packages_to_lock ->
        List.map opam_packages_to_lock ~f:(fun package ->
          Pp.text (OpamPackage.to_string package))
    in
    User_message.make
      (Pp.textf "Solution for %s:" (Path.Source.to_string_maybe_quoted lock_dir_path)
       :: parts)
  ;;
end

let opam_command_to_string_debug (args, _filter_opt) =
  List.map args ~f:(fun (simple_arg, _filter_opt) ->
    match simple_arg with
    | OpamTypes.CString s -> String.quoted s
    | CIdent ident -> ident)
  |> String.concat ~sep:" "
;;

let filter_to_blang package filter =
  let fident_to_blang packages variable string_converter =
    if Option.is_some string_converter
    then
      Code_error.raise
        "String converters in identifiers is not yet implemented"
        [ "package", Dyn.string (OpamPackage.to_string package)
        ; "filter", Dyn.string (OpamFilter.to_string filter)
        ];
    let variable_string = OpamVariable.to_string variable in
    match packages with
    | [] ->
      let pform =
        Package_variable.pform_of_opam_ident
          ~package_name:(OpamPackage.to_string package)
          variable_string
      in
      Blang.Expr (String_with_vars.make_pform Loc.none pform)
    | packages ->
      let blangs =
        List.map packages ~f:(fun package ->
          let scope =
            match package with
            | None -> Package_variable.Scope.Self
            | Some package ->
              Package (Package_name.of_string (OpamPackage.Name.to_string package))
          in
          let package_variable =
            { Package_variable.name = Package_variable.Name.of_string variable_string
            ; scope
            }
          in
          let pform = Package_variable.to_pform package_variable in
          Blang.Expr (String_with_vars.make_pform Loc.none pform))
      in
      (match blangs with
       | [ single_blang ] -> single_blang
       | _ -> Blang.And blangs)
  in
  let filter_to_sw = function
    | OpamTypes.FString string -> String_with_vars.make_text Loc.none string
    | FIdent (packages, variable, string_converter) as filter ->
      (match fident_to_blang packages variable string_converter with
       | Blang.Expr sw -> sw
       | _ ->
         User_error.raise
           [ Pp.textf
               "Expected string or identifier but found conjunction of identifiers: %s"
               (OpamFilter.to_string filter)
           ; Pp.textf
               "...while processing commands for package: %s"
               (OpamPackage.to_string package)
           ; Pp.textf "Full filter: %s" (OpamFilter.to_string filter)
           ; Pp.text
               "Note that name1+name2+name3:var is the conjunction of var for each of \
                name1, name2 and name3, i.e it is equivalent to name1:var & name2:var & \
                name3:var."
           ])
    | other ->
      Code_error.raise
        "The opam file parser shouldn't only allow identifiers and strings in places \
         where strings are expected"
        [ "package", Dyn.string (OpamPackage.to_string package)
        ; "full filter", Dyn.string (OpamFilter.to_string filter)
        ; "non-string filter", Dyn.string (OpamFilter.to_string other)
        ]
  in
  let rec filter_to_blang = function
    | OpamTypes.FBool true -> Blang.true_
    | FBool false -> Blang.false_
    | FString _ as sw_filter -> Blang.Expr (filter_to_sw sw_filter)
    | FIdent (packages, variable, string_converter) ->
      fident_to_blang packages variable string_converter
    | FOp (lhs, op, rhs) ->
      let op =
        match op with
        | `Eq -> Blang.Op.Eq
        | `Neq -> Neq
        | `Geq -> Gte
        | `Gt -> Gt
        | `Leq -> Lte
        | `Lt -> Lt
      in
      Blang.Compare (op, filter_to_sw lhs, filter_to_sw rhs)
    | FAnd (a, b) -> Blang.And [ filter_to_blang a; filter_to_blang b ]
    | FOr (a, b) -> Blang.Or [ filter_to_blang a; filter_to_blang b ]
    | FNot f -> Blang.Not (filter_to_blang f)
    | FDefined _ ->
      Code_error.raise
        "The `?` unary operator operator is not yet implemented."
        [ "package", Dyn.string (OpamPackage.to_string package)
        ; "filter", Dyn.string (OpamFilter.to_string filter)
        ]
    | FUndef _ ->
      Code_error.raise
        "Encountered undefined filter which should not be possible since no filter \
         reduction has taken place."
        [ "package", Dyn.string (OpamPackage.to_string package)
        ; "filter", Dyn.string (OpamFilter.to_string filter)
        ]
  in
  filter_to_blang filter
;;

let opam_commands_to_actions package (commands : OpamTypes.command list) =
  List.filter_map commands ~f:(fun ((args, filter) as command) ->
    let interpolate_opam_variables s =
      Re.Seq.split_full OpamFilter.string_interp_regex s
      |> Seq.map ~f:(function
        | `Text text -> `Text text
        | `Delim group ->
          (match Re.Group.get group 0 with
           | "%%" -> `Text "%"
           | interp
             when String.is_prefix ~prefix:"%{" interp
                  && String.is_suffix ~suffix:"}%" interp ->
             let ident = String.sub ~pos:2 ~len:(String.length interp - 4) interp in
             `Pform
               (Package_variable.pform_of_opam_ident
                  ~package_name:(OpamPackage.to_string package)
                  ident)
           | other ->
             User_error.raise
               [ Pp.textf
                   "Encountered malformed variable interpolation while processing \
                    commands for package %s."
                   (OpamPackage.to_string package)
               ; Pp.text "The variable interpolation:"
               ; Pp.text other
               ; Pp.text "The full command:"
               ; Pp.text (opam_command_to_string_debug command)
               ]))
      |> List.of_seq
      |> String_with_vars.make Loc.none
    in
    let terms =
      List.map args ~f:(fun (simple_arg, _filter_opt) ->
        match simple_arg with
        | OpamTypes.CString s -> interpolate_opam_variables s
        | CIdent ident ->
          String_with_vars.make_pform
            Loc.none
            (Package_variable.pform_of_opam_ident
               ~package_name:(OpamPackage.to_string package)
               ident))
    in
    match terms with
    | program :: args ->
      let action = Action.run program args in
      let action =
        match filter with
        | Some filter -> Action.When (filter_to_blang package filter, action)
        | None -> action
      in
      Some action
    | [] -> None)
;;

let opam_env_update_to_env_update ((var, env_op, value_string, _) : OpamTypes.env_update)
  : String_with_vars.t Action.Env_update.t
  =
  { Action.Env_update.op =
      (match (env_op : OpamParserTypes.env_update_op) with
       | Eq -> Eq
       | PlusEq -> PlusEq
       | EqPlus -> EqPlus
       | ColonEq -> ColonEq
       | EqColon -> EqColon
       | EqPlusEq -> EqPlusEq)
  ; var
  ; value = String_with_vars.make_text Loc.none value_string
  }
;;

let make_action = function
  | [] -> None
  | [ action ] -> Some action
  | actions -> Some (Action.Progn actions)
;;

let opam_package_to_lock_file_pkg context_for_dune ~repos ~local_packages opam_package =
  let name = OpamPackage.name opam_package in
  let version = OpamPackage.version opam_package |> OpamPackage.Version.to_string in
  let dev = OpamPackage.Name.Map.mem name local_packages in
  let opam_file =
    match OpamPackage.Name.Map.find_opt name local_packages with
    | None ->
      let opam_files =
        List.filter_map repos ~f:(fun repo ->
          Opam_repo.load_opam_package repo opam_package)
      in
      (match opam_files with
       | [ opam_file ] -> opam_file
       | _ -> Code_error.raise "Couldn't map opam package to a repository" [])
    | Some local_package -> local_package
  in
  let extra_sources =
    OpamFile.OPAM.extra_sources opam_file
    |> List.map ~f:(fun (opam_basename, opam_url) ->
      ( Path.Local.of_string (OpamFilename.Base.to_string opam_basename)
      , let url = Loc.none, OpamUrl.to_string (OpamFile.URL.url opam_url) in
        let checksum =
          match OpamFile.URL.checksum opam_url with
          | [] -> None
          (* opam discards the later checksums, so we only take the first one *)
          | checksum :: _ -> Some (Loc.none, Checksum.of_opam_hash checksum)
        in
        Lock_dir.Source.Fetch { Lock_dir.Source.url; checksum } ))
  in
  let info =
    { Lock_dir.Pkg_info.name = Package_name.of_string (OpamPackage.Name.to_string name)
    ; version
    ; dev
    ; source = None
    ; extra_sources
    }
  in
  (* This will collect all the atoms from the package's dependency formula regardless of conditions *)
  let deps =
    let deps =
      Context_for_dune.filter_deps context_for_dune opam_package opam_file.depends
    in
    OpamFormula.fold_right (fun acc (name, _condition) -> name :: acc) [] deps
    |> List.map ~f:(fun name ->
      Loc.none, Package_name.of_string (OpamPackage.Name.to_string name))
  in
  let build_env action =
    let env_update =
      OpamFile.OPAM.build_env opam_file |> List.map ~f:opam_env_update_to_env_update
    in
    match env_update with
    | [] -> action
    | env_update -> Action.Withenv (env_update, action)
  in
  let build_command =
    let subst_step =
      OpamFile.OPAM.substs opam_file
      |> List.map ~f:(fun x ->
        let x = OpamFilename.Base.to_string x in
        let input = String_with_vars.make_text Loc.none (x ^ ".in") in
        let output = String_with_vars.make_text Loc.none x in
        Action.Substitute (input, output))
    in
    let patch_step =
      OpamFile.OPAM.patches opam_file
      |> List.map ~f:(fun (basename, filter) ->
        let action =
          Action.Patch
            (String_with_vars.make_text Loc.none (OpamFilename.Base.to_string basename))
        in
        match filter with
        | None -> action
        | Some filter -> Action.When (filter_to_blang opam_package filter, action))
    in
    let build_step =
      opam_commands_to_actions opam_package (OpamFile.OPAM.build opam_file)
    in
    List.concat [ subst_step; patch_step; build_step ]
    |> make_action
    |> Option.map ~f:build_env
  in
  let install_command =
    OpamFile.OPAM.install opam_file
    |> opam_commands_to_actions opam_package
    |> make_action
    |> Option.map ~f:build_env
  in
  let exported_env =
    OpamFile.OPAM.env opam_file |> List.map ~f:opam_env_update_to_env_update
  in
  { Lock_dir.Pkg.build_command; install_command; deps; info; exported_env }
;;

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
      Code_error.raise
        "Unexpected exception raised while solving dependencies"
        [ "exception", Exn.to_dyn unexpected_exn ]
  in
  match result with
  | Error e -> Error (`Diagnostic_message (Solver.diagnostics e |> Pp.text))
  | Ok packages -> Ok (Solver.packages_of_result packages)
;;

(* Scan a path recursively down retrieving a list of all files together with their
   relative path. *)
let scan_files_entries path =
  (* TODO Add some cycle detection *)
  let rec read acc dir =
    let path = Path.append_local path dir in
    match Path.readdir_unsorted_with_kinds path with
    | Ok entries ->
      List.fold_left entries ~init:acc ~f:(fun acc (filename, kind) ->
        let local_path = Path.Local.relative dir filename in
        match (kind : Unix.file_kind) with
        | S_REG -> local_path :: acc
        | S_DIR -> read acc local_path
        | _ ->
          (* TODO should be an error *)
          acc)
    | Error (Unix.ENOENT, _, _) -> acc
    | Error err ->
      User_error.raise
        [ Pp.text "Unable to read file in opam repository:"; Unix_error.Detailed.pp err ]
  in
  read [] Path.Local.root
  |> List.map ~f:(fun local_file ->
    { Lock_dir.Write_disk.Files_entry.original_file = Path.append_local path local_file
    ; local_file
    })
;;

module Solver_result = struct
  type t =
    { summary : Summary.t
    ; lock_dir : Lock_dir.t
    ; files : Lock_dir.Write_disk.Files_entry.t Package_name.Map.Multi.t
    }
end

let solve_lock_dir solver_env version_preference repos ~local_packages =
  let is_local_package package =
    OpamPackage.Name.Map.mem (OpamPackage.name package) local_packages
  in
  let context =
    Context_for_dune.create ~solver_env ~repos ~version_preference ~local_packages
  in
  solve_package_list local_packages context
  |> Result.map ~f:(fun solution ->
    (* don't include local packages in the lock dir *)
    let opam_packages_to_lock = List.filter solution ~f:(Fun.negate is_local_package) in
    let summary = { Summary.opam_packages_to_lock } in
    let lock_dir =
      match
        Package_name.Map.of_list_map opam_packages_to_lock ~f:(fun opam_package ->
          let pkg =
            opam_package_to_lock_file_pkg context ~repos ~local_packages opam_package
          in
          pkg.info.name, pkg)
      with
      | Error (name, _pkg1, _pkg2) ->
        Code_error.raise
          (sprintf
             "Solver selected multiple packages named \"%s\""
             (Package_name.to_string name))
          []
      | Ok pkgs_by_name ->
        Lock_dir.create_latest_version pkgs_by_name ~ocaml:None ~repos:(Some repos)
    in
    let files =
      opam_packages_to_lock
      |> List.filter_map ~f:(fun opam_package ->
        let files_path =
          List.find_map repos ~f:(fun repo ->
            Opam_repo.get_opam_package_files_path repo opam_package)
        in
        match files_path with
        | None -> None
        | Some files_path ->
          (match scan_files_entries files_path with
           | [] -> None
           | files ->
             Some
               ( Package_name.of_string
                   (OpamPackage.Name.to_string (OpamPackage.name opam_package))
               , files )))
      |> Package_name.Map.of_list_exn
    in
    { Solver_result.summary; lock_dir; files })
;;
