open Import
open Fiber.O
module With_file = Opam_repo.With_file

module Monad : Opam_0install.S.Monad with type 'a t = 'a Fiber.t = struct
  type 'a t = 'a Fiber.t

  module O = Fiber.O

  let return a = Fiber.return a

  module Seq = struct
    let parallel_map f t =
      Fiber.parallel_map (List.of_seq t) ~f |> Fiber.map ~f:List.to_seq
    ;;
  end

  module List = struct
    let iter f x = Fiber.sequential_iter x ~f
    let iter2 f x y = Fiber.sequential_iter (List.combine x y) ~f:(fun (x, y) -> f x y)
  end
end

module Context_for_dune = struct
  type 'a monad = 'a Monad.t
  type filter = OpamTypes.filter
  type rejection = Unavailable

  let local_package_default_version =
    Package_version.to_opam_package_version Lock_dir.Pkg_info.default_version
  ;;

  type candidates =
    { resolved : With_file.t OpamPackage.Version.Map.t
    ; available : (OpamTypes.version * (OpamFile.OPAM.t, rejection) result) list
    }

  type t =
    { repos : Opam_repo.t list
    ; version_preference : Version_preference.t
    ; local_packages : OpamFile.OPAM.t Package_name.Map.t
    ; solver_env : Solver_env.t
    ; dune_version : OpamPackage.Version.t
    ; stats_updater : Solver_stats.Updater.t
    ; candidates_cache : (Package_name.t, candidates) Table.t
    ; (* The solver can call this function several times on the same package.
         If the package contains an invalid "available" filter we want to print a
         warning, but only once per package. This field will keep track of the
         packages for which we've printed a warning. *)
      mutable available_cache : bool OpamPackage.Map.t
    }

  let create ~solver_env ~repos ~local_packages ~version_preference ~stats_updater =
    let dune_version =
      let major, minor = Dune_lang.Stanza.latest_version in
      OpamPackage.Version.of_string @@ sprintf "%d.%d" major minor
    in
    let candidates_cache = Table.create (module Package_name) 1 in
    { repos
    ; version_preference
    ; local_packages
    ; solver_env
    ; dune_version
    ; stats_updater
    ; candidates_cache
    ; available_cache = OpamPackage.Map.empty
    }
  ;;

  let pp_rejection f = function
    | Unavailable -> Format.pp_print_string f "Availability condition not satisfied"
  ;;

  let opam_version_compare =
    let opam_file_compare_by_version =
      let opam_package_version_compare a b =
        OpamPackage.Version.compare a b |> Ordering.of_int
      in
      fun a b ->
        opam_package_version_compare (OpamFile.OPAM.version a) (OpamFile.OPAM.version b)
    in
    fun t ->
      match t.version_preference with
      | Oldest -> opam_file_compare_by_version
      | Newest -> Ordering.reverse opam_file_compare_by_version
  ;;

  let eval_to_bool (filter : filter) : (bool, [> `Not_a_bool of string ]) result =
    try Ok (OpamFilter.eval_to_bool ~default:false (Fun.const None) filter) with
    | Invalid_argument msg -> Error (`Not_a_bool msg)
  ;;

  let is_opam_available t opam =
    let package = OpamFile.OPAM.package opam in
    match OpamPackage.Map.find_opt package t.available_cache with
    | Some s -> s
    | None ->
      let res =
        let available = OpamFile.OPAM.available opam in
        match
          let available_vars_resolved =
            Resolve_opam_formula.substitute_variables_in_filter
              ~stats_updater:(Some t.stats_updater)
              t.solver_env
              available
          in
          eval_to_bool available_vars_resolved
        with
        | Ok available -> available
        | Error (`Not_a_bool msg) ->
          (let package_string = OpamFile.OPAM.package opam |> OpamPackage.to_string in
           let available_string = OpamFilter.to_string available in
           User_warning.emit
             [ Pp.textf
                 "Ignoring package %s as its \"available\" filter can't be resolved to a \
                  boolean value."
                 package_string
             ; Pp.textf "available: %s" available_string
             ; Pp.text msg
             ]);
          false
      in
      t.available_cache <- OpamPackage.Map.add package res t.available_cache;
      res
  ;;

  let candidates t name =
    let* () = Fiber.return () in
    let key = Package_name.of_opam_package_name name in
    match Package_name.Map.find t.local_packages key with
    | Some local_package ->
      let version =
        Option.value local_package.version ~default:local_package_default_version
      in
      Fiber.return [ version, Ok local_package ]
    | None ->
      let+ res =
        match Table.find t.candidates_cache key with
        | Some res -> Fiber.return res
        | None ->
          let+ resolved = Opam_repo.load_all_versions t.repos name in
          let available =
            (* The CONTEXT interface doesn't give us a way to report this type of
               error and there's not enough context to give a helpful error message
               so just tell opam_0install that there are no versions of this
               package available (technically true) and let it produce the error
               message. *)
            OpamPackage.Version.Map.values resolved
            |> List.sort ~compare:(fun p1 p2 ->
              opam_version_compare t (With_file.opam_file p1) (With_file.opam_file p2))
            |> List.map ~f:(fun with_file ->
              let opam_file = With_file.opam_file with_file in
              let opam_file_result =
                if is_opam_available t opam_file then Ok opam_file else Error Unavailable
              in
              OpamFile.OPAM.version opam_file, opam_file_result)
          in
          let res = { available; resolved } in
          Table.set t.candidates_cache key res;
          res
      in
      res.available
  ;;

  let user_restrictions : t -> OpamPackage.Name.t -> OpamFormula.version_constraint option
    =
    let dune = OpamPackage.Name.of_string "dune" in
    fun t pkg ->
      if OpamPackage.Name.equal dune pkg then Some (`Eq, t.dune_version) else None
  ;;

  let filter_deps t package filtered_formula =
    let package_is_local =
      OpamPackage.name package
      |> Package_name.of_opam_package_name
      |> Package_name.Map.mem t.local_packages
    in
    Resolve_opam_formula.apply_filter
      ~stats_updater:(Some t.stats_updater)
      ~with_test:package_is_local
      t.solver_env
      filtered_formula
  ;;
end

module Solver = Opam_0install.Solver.Make (Monad) (Context_for_dune)

let is_valid_global_variable_name = function
  | "root" -> false
  | _ -> true
;;

let is_valid_package_variable_name = function
  | "hash" | "build-id" | "misc" | "opam-version" | "depends" | "build" | "opamfile" ->
    false
  | _ -> true
;;

let invalid_variable_error ~loc variable =
  User_error.raise
    ~loc
    [ Pp.textf "Variable %S is not supported." (OpamVariable.to_string variable) ]
;;

let opam_variable_to_slang ~loc packages variable =
  let variable_string = OpamVariable.to_string variable in
  let convert_with_package_name package_name =
    if not (is_valid_package_variable_name variable_string)
    then invalid_variable_error ~loc variable;
    let pform =
      let name = Package_variable.Name.of_string variable_string in
      let scope : Package_variable.Scope.t =
        match package_name with
        | None -> Self
        | Some p -> Package (Package_name.of_opam_package_name p)
      in
      Package_variable.to_pform { Package_variable.name; scope }
    in
    Slang.pform pform
  in
  match packages with
  | [] ->
    if not (is_valid_global_variable_name variable_string)
    then
      (* Note that there's no syntactic distinction between global variables
         and package variables in the current package. This check will prevent
         invalid global variable names from being used for package variables in the
         current package where the optional qualifier "_:" is omitted. *)
      invalid_variable_error ~loc variable;
    (match Pform.Var.of_opam_global_variable_name variable_string with
     | Some global_var -> Slang.pform (Pform.Var global_var)
     | None -> convert_with_package_name None)
  | [ package_name ] -> convert_with_package_name package_name
  | many ->
    Slang.blang
      (Blang.And
         (List.map many ~f:(fun package_name ->
            Blang.Expr (convert_with_package_name package_name))))
;;

let opam_fident_to_slang ~loc fident =
  let packages, variable, string_converter = OpamFilter.desugar_fident fident in
  let slang = opam_variable_to_slang ~loc packages variable in
  match string_converter with
  | None -> slang
  | Some (then_, else_) ->
    (* The "else" case is also used when evaluating the condition would expand
       an undefined variable. The catch_undefined_var operator is used to
       convert expressions that throw undefined variable exceptions into false.
    *)
    let condition =
      Blang.Expr (Slang.catch_undefined_var slang ~fallback:(Slang.bool false))
    in
    Slang.if_ condition ~then_:(Slang.text then_) ~else_:(Slang.text else_)
;;

let opam_raw_fident_to_slang ~loc raw_ident =
  OpamTypesBase.filter_ident_of_string raw_ident |> opam_fident_to_slang ~loc
;;

let opam_string_to_slang ~package ~loc opam_string =
  Re.Seq.split_full OpamFilter.string_interp_regex opam_string
  |> Seq.map ~f:(function
    | `Text text -> Slang.text text
    | `Delim group ->
      (match Re.Group.get group 0 with
       | "%%" -> Slang.text "%"
       | interp
         when String.is_prefix ~prefix:"%{" interp && String.is_suffix ~suffix:"}%" interp
         ->
         let ident = String.sub ~pos:2 ~len:(String.length interp - 4) interp in
         opam_raw_fident_to_slang ~loc ident
       | other ->
         User_error.raise
           ~loc
           [ Pp.textf
               "Encountered malformed variable interpolation while processing commands \
                for package %s."
               (OpamPackage.to_string package)
           ; Pp.text "The variable interpolation:"
           ; Pp.text other
           ]))
  |> List.of_seq
  |> Slang.concat
;;

(* Translate an Opam filter into Dune's "Slang" DSL. The main difference between
   the two languages is in their treatment of undefined package variables. In
   Opam filters, undefined variables take on the value <undefined> which
   is "falsey" in some contexts and propagates through boolean operators if
   their result could be affected by the <undefined> term. Slang doesn't have an
   <undefined> value but raises an exception when an undefined variable is
   expanded. There are two operators in Slang for handling exceptions:

   - "(has_undefined_var <arg>)" evaluates <arg>, discarding the result, and
     returns a boolean which is true iff evaluating <arg> failed due to an
     undefined variable
   - "(catch_undefined_var <value> <fallback>)" evaluates <value> and returns
     the result unless evaluation failed due to an undefined variable, in which
     case the result of <fallback> is returned

   These two Slang operators are used to emulate Opam's undefined value
   semantics.
*)
let filter_to_blang ~package ~loc filter =
  let filter_to_slang = function
    | OpamTypes.FString s -> opam_string_to_slang ~package ~loc s
    | FIdent fident -> opam_fident_to_slang ~loc fident
    | other ->
      Code_error.raise
        "The opam file parser should only allow identifiers and strings in places where \
         strings are expected"
        [ "package", Dyn.string (OpamPackage.to_string package)
        ; "full filter", Dyn.string (OpamFilter.to_string filter)
        ; "non-string filter", Dyn.string (OpamFilter.to_string other)
        ]
  in
  let rec filter_to_blang = function
    | OpamTypes.FBool true -> Blang.Ast.true_
    | FBool false -> Blang.Ast.false_
    | (FString _ | FIdent _) as slangable -> Blang.Expr (filter_to_slang slangable)
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
      Blang.Compare (op, filter_to_slang lhs, filter_to_slang rhs)
    | FAnd (lhs, rhs) ->
      Blang.Expr
        (Slang.and_absorb_undefined_var [ filter_to_blang lhs; filter_to_blang rhs ])
    | FOr (lhs, rhs) ->
      Blang.Expr
        (Slang.or_absorb_undefined_var [ filter_to_blang lhs; filter_to_blang rhs ])
    | FNot f -> Blang.Not (filter_to_blang f)
    | FDefined f ->
      let blang = filter_to_blang f in
      Blang.Not (Blang.Expr (Slang.has_undefined_var (Slang.blang blang)))
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

let opam_commands_to_actions loc package (commands : OpamTypes.command list) =
  List.filter_map commands ~f:(fun (args, filter) ->
    let terms =
      List.map args ~f:(fun (simple_arg, filter_opt) ->
        let slang =
          match simple_arg with
          | OpamTypes.CString s -> opam_string_to_slang ~package ~loc s
          | CIdent ident -> opam_raw_fident_to_slang ~loc ident
        in
        let slang = Slang.simplify slang in
        match filter_opt with
        | None -> slang
        | Some filter ->
          let filter_blang =
            filter_to_blang ~package ~loc filter |> Slang.simplify_blang
          in
          Slang.when_ filter_blang slang)
    in
    if List.is_empty terms
    then None
    else (
      let action =
        let action = Action.Run terms in
        match filter with
        | None -> action
        | Some filter ->
          let condition = filter_to_blang ~package ~loc filter |> Slang.simplify_blang in
          Action.When (condition, action)
      in
      Some action))
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

let remove_filters_from_command ((command, _filter) : OpamTypes.command)
  : OpamTypes.command
  =
  let command = List.map command ~f:(fun (term, _filter) -> term, None) in
  command, None
;;

let remove_filters_from_opam_file opam_file =
  opam_file
  |> OpamFile.OPAM.with_build
       (List.map ~f:remove_filters_from_command (OpamFile.OPAM.build opam_file))
  |> OpamFile.OPAM.with_install
       (List.map ~f:remove_filters_from_command (OpamFile.OPAM.install opam_file))
;;

let opam_package_to_lock_file_pkg
  solver_env
  version_by_package_name
  ~experimental_translate_opam_filters
  opam_package
  ~(candidates_cache : (Package_name.t, Context_for_dune.candidates) Table.t)
  =
  let name = OpamPackage.name opam_package in
  let version =
    OpamPackage.version opam_package |> Package_version.of_opam_package_version
  in
  let opam_file, loc =
    let with_file =
      (let key = Package_name.of_opam_package_name name in
       Table.find_exn candidates_cache key)
        .resolved
      |> OpamPackage.Version.Map.find (Package_version.to_opam_package_version version)
    in
    let opam_file =
      let opam_file_with_filters = With_file.opam_file with_file in
      if experimental_translate_opam_filters
      then opam_file_with_filters
      else remove_filters_from_opam_file opam_file_with_filters
    in
    let loc = Loc.in_file (With_file.file with_file) in
    opam_file, loc
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
    let source =
      OpamFile.OPAM.url opam_file
      |> Option.map ~f:(fun (url : OpamFile.URL.t) ->
        let checksum =
          OpamFile.URL.checksum url
          |> List.hd_opt
          |> Option.map ~f:(fun hash -> Loc.none, Checksum.of_opam_hash hash)
        in
        let url =
          let url = OpamFile.URL.url url in
          Loc.none, OpamUrl.to_string url
        in
        Lock_dir.Source.Fetch { url; checksum })
    in
    { Lock_dir.Pkg_info.name = Package_name.of_opam_package_name name
    ; version
    ; dev = false
    ; source
    ; extra_sources
    }
  in
  let deps =
    match
      Resolve_opam_formula.filtered_formula_to_package_names
        ~stats_updater:None
        ~with_test:false
        solver_env
        version_by_package_name
        opam_file.depends
    with
    | Ok dep_package_names ->
      List.map dep_package_names ~f:(fun package_name -> Loc.none, package_name)
    | Error (`Formula_could_not_be_satisfied hints) ->
      Code_error.raise
        "Dependencies of package can't be satisfied from packages in solution"
        [ "package", Dyn.string (OpamFile.OPAM.package opam_file |> OpamPackage.to_string)
        ; "hints", Dyn.list Resolve_opam_formula.Unsatisfied_formula_hint.to_dyn hints
        ]
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
        | Some filter ->
          Action.When
            ( filter_to_blang ~package:opam_package ~loc:Loc.none filter
              |> Slang.simplify_blang
            , action ))
    in
    let build_step =
      opam_commands_to_actions loc opam_package (OpamFile.OPAM.build opam_file)
    in
    List.concat [ subst_step; patch_step; build_step ]
    |> make_action
    |> Option.map ~f:build_env
  in
  let install_command =
    OpamFile.OPAM.install opam_file
    |> opam_commands_to_actions loc opam_package
    |> make_action
    |> Option.map ~f:build_env
  in
  let exported_env =
    OpamFile.OPAM.env opam_file |> List.map ~f:opam_env_update_to_env_update
  in
  let kind =
    if List.mem (OpamFile.OPAM.flags opam_file) ~equal:Poly.equal Pkgflag_Compiler
    then `Compiler
    else `Non_compiler
  in
  kind, { Lock_dir.Pkg.build_command; install_command; deps; info; exported_env }
;;

let solve_package_list ~local_package_names context =
  let* result =
    Fiber.collect_errors (fun () ->
      (* [Solver.solve] returns [Error] when it's unable to find a solution to
         the dependencies, but can also raise exceptions, for example if opam
         is unable to parse an opam file in the package repository. To prevent
         an unexpected opam exception from crashing dune, we catch all
         exceptions raised by the solver and report them as [User_error]s
         instead. *)
      Solver.solve context local_package_names)
    >>| function
    | Ok (Ok res) -> Ok res
    | Ok (Error e) -> Error (`Diagnostics e)
    | Error [] -> assert false
    | Error (exn :: _) ->
      (* CR-rgrinberg: this needs to be handled right *)
      Error (`Exn exn.exn)
  in
  match result with
  | Error (`Exn exn) ->
    (match exn with
     | OpamPp.(Bad_format _ | Bad_format_list _ | Bad_version _) as bad_format ->
       User_error.raise [ Pp.text (OpamPp.string_of_bad_format bad_format) ]
     | unexpected_exn ->
       Code_error.raise
         "Unexpected exception raised while solving dependencies"
         [ "exception", Exn.to_dyn unexpected_exn ])
  | Error (`Diagnostics e) ->
    let+ diagnostics = Solver.diagnostics e in
    Error (`Diagnostic_message (Pp.text diagnostics))
  | Ok packages -> Fiber.return @@ Ok (Solver.packages_of_result packages)
;;

module Solver_result = struct
  type t =
    { lock_dir : Lock_dir.t
    ; files : File_entry.t Package_name.Map.Multi.t
    }
end

let solve_lock_dir
  solver_env
  version_preference
  repos
  ~local_packages
  ~experimental_translate_opam_filters
  =
  let* solver_result =
    let stats_updater = Solver_stats.Updater.init () in
    let context =
      Context_for_dune.create
        ~solver_env
        ~repos
        ~version_preference
        ~local_packages:
          (Package_name.Map.map local_packages ~f:Local_package.For_solver.to_opam_file)
        ~stats_updater
    in
    solve_package_list
      context
      ~local_package_names:
        (Package_name.Map.to_list_map local_packages ~f:(fun name _ ->
           Package_name.to_opam_package_name name))
    >>| Result.map ~f:(fun solution ->
      let version_by_package_name =
        List.map solution ~f:(fun (package : OpamPackage.t) ->
          ( Package_name.of_opam_package_name (OpamPackage.name package)
          , Package_version.of_string (OpamPackage.version_to_string package) ))
        |> Package_name.Map.of_list_exn
      in
      (* don't include local packages in the lock dir *)
      let opam_packages_to_lock =
        let is_local_package package =
          OpamPackage.name package
          |> Package_name.of_opam_package_name
          |> Package_name.Map.mem local_packages
        in
        List.filter solution ~f:(Fun.negate is_local_package)
      in
      let ocaml, pkgs =
        let pkgs =
          List.map opam_packages_to_lock ~f:(fun opam_package ->
            opam_package_to_lock_file_pkg
              solver_env
              version_by_package_name
              ~experimental_translate_opam_filters
              opam_package
              ~candidates_cache:context.candidates_cache)
        in
        let ocaml =
          (* This doesn't allow the compiler to live in the source tree. Oh
             well, it's not possible now anyway. *)
          match
            List.filter_map pkgs ~f:(fun (kind, pkg) ->
              match kind with
              | `Compiler -> Some pkg.info.name
              | `Non_compiler -> None)
          with
          | [] -> None
          | [ x ] -> Some (Loc.none, x)
          | _ ->
            User_error.raise
              [ Pp.text "multiple compilers selected" ]
              ~hints:[ Pp.text "add a conflict" ]
        in
        let pkgs =
          Package_name.Map.of_list_map pkgs ~f:(fun (_kind, pkg) -> pkg.info.name, pkg)
        in
        ocaml, pkgs
      in
      let lock_dir =
        match pkgs with
        | Error (name, _pkg1, _pkg2) ->
          Code_error.raise
            "Solver selected multiple versions for the same package"
            [ "name", Package_name.to_dyn name ]
        | Ok pkgs_by_name ->
          let stats = Solver_stats.Updater.snapshot stats_updater in
          let expanded_solver_variable_bindings =
            Solver_stats.Expanded_variable_bindings.of_variable_set
              stats.expanded_variables
              solver_env
          in
          Lock_dir.create_latest_version
            pkgs_by_name
            ~local_packages:(Package_name.Map.values local_packages)
            ~ocaml
            ~repos:(Some repos)
            ~expanded_solver_variable_bindings
      in
      let+ files =
        let with_files =
          List.map opam_packages_to_lock ~f:(fun opam_package ->
            let package_name =
              OpamPackage.name opam_package
              |> OpamPackage.Name.to_string
              |> Package_name.of_string
            in
            let candidates = Table.find_exn context.candidates_cache package_name in
            OpamPackage.Version.Map.find
              (OpamPackage.version opam_package)
              candidates.resolved)
        in
        Opam_repo.get_opam_package_files with_files
        >>| List.map2 with_files ~f:(fun with_file entries ->
          let package_name =
            With_file.package with_file
            |> OpamPackage.name
            |> OpamPackage.Name.to_string
            |> Package_name.of_string
          in
          package_name, entries)
        >>| List.filter ~f:(fun (_, entries) -> List.is_non_empty entries)
        >>| Package_name.Map.of_list_exn
      in
      { Solver_result.lock_dir; files })
  in
  match solver_result with
  | Error _ as e -> Fiber.return e
  | Ok ok ->
    let+ ok = ok in
    Ok ok
;;
