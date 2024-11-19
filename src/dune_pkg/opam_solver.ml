open Import
open Fiber.O

let add_self_to_filter_env package env variable =
  match OpamVariable.Full.scope variable with
  | Self | Package _ -> env variable
  | Global ->
    let var_name = Package_variable_name.of_opam (OpamVariable.Full.variable variable) in
    if Package_variable_name.(equal var_name name)
    then Some (OpamVariable.S (OpamPackage.Name.to_string (OpamPackage.name package)))
    else if Package_variable_name.(equal var_name version)
    then Some (S (OpamPackage.Version.to_string (OpamPackage.version package)))
    else env variable
;;

module Priority = struct
  (* A priority defines a package's position in the list of candidates
     fed to the solver. Any change to package selection should be reflected in
     this priority rather than implemented in an ad-hoc manner *)
  type t =
    { (* We don't really need this field, since we filter avoid-version
         packages. If this changes, we still prefer packages
         with [avoid-version: false] *)
      avoid : bool
    ; version : OpamPackage.Version.t
    }

  let compare_version =
    let ord x y = OpamPackage.Version.compare x y |> Ordering.of_int in
    fun (pref : Version_preference.t) x y ->
      match pref with
      | Oldest -> ord x y
      | Newest -> ord y x
  ;;

  let compare pref t { avoid; version } =
    Tuple.T2.compare
      Bool.compare
      (compare_version pref)
      (t.avoid, t.version)
      (avoid, version)
  ;;

  let make (package : OpamFile.OPAM.t) =
    let avoid = List.mem package.flags Pkgflag_AvoidVersion ~equal:Poly.equal in
    let version = OpamFile.OPAM.package package |> OpamPackage.version in
    { version; avoid }
  ;;
end

module Context_for_dune = struct
  type filter = OpamTypes.filter

  type rejection =
    | (* TODO proper error messages for packages skipped via avoid-version *)
      Unavailable
    | Avoid_version

  let local_package_default_version =
    Package_version.to_opam_package_version Lock_dir.Pkg_info.default_version
  ;;

  type candidates =
    { resolved : Resolved_package.t OpamPackage.Version.Map.t
    ; available : (OpamTypes.version * (OpamFile.OPAM.t, rejection) result) list
    }

  type t =
    { repos : Opam_repo.t list
    ; version_preference : Version_preference.t
    ; pinned_packages : Resolved_package.t Package_name.Map.t
    ; local_packages : OpamFile.OPAM.t Package_name.Map.t
    ; solver_env : Solver_env.t
    ; dune_version : OpamPackage.Version.t
    ; stats_updater : Solver_stats.Updater.t
    ; candidates_cache : (Package_name.t, candidates) Fiber_cache.t
    ; (* The solver can call this function several times on the same package.
         If the package contains an invalid "available" filter we want to print a
         warning, but only once per package. This field will keep track of the
         packages for which we've printed a warning. *)
      available_cache : (OpamPackage.t, bool) Table.t
    ; constraints : OpamTypes.filtered_formula Package_name.Map.t
    }

  let create
    ~pinned_packages
    ~solver_env
    ~repos
    ~local_packages
    ~version_preference
    ~stats_updater
    ~constraints
    =
    let candidates_cache = Fiber_cache.create (module Package_name) in
    let constraints =
      List.map constraints ~f:(fun (constraint_ : Package_dependency.t) ->
        constraint_.name, constraint_)
      |> Package_name.Map.of_list_multi
      |> Package_name.Map.map ~f:Package_dependency.list_to_opam_filtered_formula
    in
    let available_cache =
      Table.create
        (module struct
          include OpamPackage

          let to_dyn = Opam_dyn.package
        end)
        1
    in
    { repos
    ; version_preference
    ; local_packages
    ; pinned_packages
    ; solver_env
    ; dune_version = Dune_dep.version
    ; stats_updater
    ; candidates_cache
    ; available_cache
    ; constraints
    }
  ;;

  let pp_rejection = function
    | Unavailable -> Pp.paragraph "Availability condition not satisfied"
    | Avoid_version -> Pp.paragraph "Package is excluded by avoid-version"
  ;;

  let eval_to_bool (filter : filter) : (bool, [> `Not_a_bool of string ]) result =
    try Ok (OpamFilter.eval_to_bool ~default:false (Fun.const None) filter) with
    | Invalid_argument msg -> Error (`Not_a_bool msg)
  ;;

  let is_opam_available t opam =
    let package = OpamFile.OPAM.package opam in
    Table.find_or_add t.available_cache package ~f:(fun (_ : OpamPackage.t) ->
      let available = OpamFile.OPAM.available opam in
      match
        let available_vars_resolved =
          OpamFilter.partial_eval
            (add_self_to_filter_env
               package
               (Solver_stats.Updater.wrap_env
                  t.stats_updater
                  (Solver_env.to_env t.solver_env)))
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
        false)
  ;;

  let available_or_error t opam_file =
    (* The CONTEXT interface doesn't give us a way to report this type of
       error and there's not enough context to give a helpful error message
       so just tell opam_0install that there are no versions of this
       package available (technically true) and let it produce the error
       message. *)
    if is_opam_available t opam_file then Ok opam_file else Error Unavailable
  ;;

  let pinned_candidate t resolved_package =
    let version = Resolved_package.package resolved_package |> OpamPackage.version in
    let available =
      (* We don't respect avoid-version for pinned packages. This is
         intentional. *)
      [ version, Resolved_package.opam_file resolved_package |> available_or_error t ]
    in
    let resolved = OpamPackage.Version.Map.singleton version resolved_package in
    { available; resolved }
  ;;

  let repo_candidate t name =
    let+ resolved = Opam_repo.load_all_versions t.repos name in
    let available =
      OpamPackage.Version.Map.values resolved
      |> List.map ~f:(fun p -> p, Priority.make (Resolved_package.opam_file p))
      (* Note that although the packages are taken from a map,
         explicitly sorting them is still necessary. This sort applies
         the configured version preference and also allows the solver to
         prefer versions without the avoid-version flag set. *)
      |> List.sort ~compare:(fun (_, x) (_, y) ->
        Priority.compare t.version_preference x y)
      |> List.map ~f:(fun (resolved_package, (priority : Priority.t)) ->
        let opam_file = Resolved_package.opam_file resolved_package in
        let opam_file_result =
          if priority.avoid then Error Avoid_version else available_or_error t opam_file
        in
        OpamFile.OPAM.version opam_file, opam_file_result)
    in
    { available; resolved }
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
        Fiber_cache.find_or_add t.candidates_cache key ~f:(fun () ->
          match Package_name.Map.find t.pinned_packages key with
          | Some resolved_package -> Fiber.return (pinned_candidate t resolved_package)
          | None -> repo_candidate t name)
      in
      res.available
  ;;

  let user_restrictions : t -> OpamPackage.Name.t -> OpamFormula.version_constraint option
    =
    fun t pkg ->
    if Package_name.equal Dune_dep.name (Package_name.of_opam_package_name pkg)
    then Some (`Eq, t.dune_version)
    else None
  ;;

  let filter_deps t package filtered_formula =
    let name = OpamPackage.name package |> Package_name.of_opam_package_name in
    (* Add additional constraints to the formula. This works in two steps.
       First identify all the additional constraints applied to packages which
       appear in the current package's dependency formula. Then each additional
       constnraint is and-ed with the current package's dependency formula. *)
    let filtered_formula =
      OpamFormula.fold_left
        (fun additional_formulae (pkg, _) ->
          let name = Package_name.of_opam_package_name pkg in
          match Package_name.Map.find t.constraints name with
          | None -> additional_formulae
          | Some additional -> additional :: additional_formulae)
        []
        filtered_formula
      |> List.fold_left ~init:filtered_formula ~f:(fun additional acc ->
        OpamFormula.And (acc, additional))
    in
    let package_is_local = Package_name.Map.mem t.local_packages name in
    Resolve_opam_formula.apply_filter
      (add_self_to_filter_env
         package
         (Solver_stats.Updater.wrap_env t.stats_updater (Solver_env.to_env t.solver_env)))
      ~with_test:package_is_local
      filtered_formula
  ;;
end

module Solver = Opam_0install.Solver.Make (Context_for_dune)

let is_valid_global_variable_name = function
  | "root" -> false
  | _ -> true
;;

(* CR-rgrinberg: we need this validation in substitution actions as well *)
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
      let name = Package_variable_name.of_string variable_string in
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
      let op = Package_dependency.Constraint.Op.of_opam op in
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

let simplify_filter get_solver_var =
  OpamFilter.partial_eval (fun var ->
    match OpamVariable.Full.scope var with
    | Global ->
      let name = OpamVariable.Full.variable var |> Package_variable_name.of_opam in
      if Package_variable_name.equal name Package_variable_name.with_test
      then
        (* We don't generate lockfiles for local packages, and we don't include
           test dependencies for non-local packages, so "with-test" always
           evaluates to "false". *)
        Some (B false)
      else get_solver_var name |> Option.map ~f:Variable_value.to_opam_variable_contents
    | _ -> None)
;;

let partial_eval_filter = function
  | None -> `Filter None
  | Some f ->
    let env = Fun.const None in
    (match OpamFilter.eval_to_bool env f with
     | exception Failure _ -> `Filter (Some f)
     | b -> if b then `Filter None else `Skip)
;;

let opam_commands_to_actions
  get_solver_var
  loc
  package
  (commands : OpamTypes.command list)
  =
  List.filter_map commands ~f:(fun (args, filter) ->
    let filter = Option.map filter ~f:(simplify_filter get_solver_var) in
    match partial_eval_filter filter with
    | `Skip -> None
    | `Filter filter ->
      let terms =
        List.filter_map args ~f:(fun (simple_arg, filter) ->
          let filter = Option.map filter ~f:(simplify_filter get_solver_var) in
          match partial_eval_filter filter with
          | `Skip -> None
          | `Filter filter ->
            let slang =
              let slang =
                match simple_arg with
                | OpamTypes.CString s -> opam_string_to_slang ~package ~loc s
                | CIdent ident -> opam_raw_fident_to_slang ~loc ident
              in
              Slang.simplify slang
            in
            Some
              (Slang.simplify
                 (match filter with
                  | None -> slang
                  | Some filter ->
                    let filter_blang =
                      filter_to_blang ~package ~loc filter |> Slang.simplify_blang
                    in
                    Slang.when_ filter_blang slang)))
      in
      if List.is_empty terms
      then None
      else (
        let action =
          let action = Action.Run terms in
          match filter with
          | None -> action
          | Some filter ->
            let condition =
              filter_to_blang ~package ~loc filter |> Slang.simplify_blang
            in
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

(* Heuristic to determine whether a package is an ocaml compiler *)
let opam_file_is_compiler (opam_package : OpamFile.OPAM.t) =
  (* Identify compiler packages by using the fact that all compiler
     Packages declare conflicts with other compiler packages. note
     that relying on the "compiler" flag to identify compiler packages
     will not work, as compiler options packages (such as
     ocaml-option-flambda) also have this flag. *)
  let ocaml_core_compiler = OpamPackage.Name.of_string "ocaml-core-compiler" in
  List.mem opam_package.conflict_class ocaml_core_compiler ~equal:OpamPackage.Name.equal
;;

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

let opam_package_to_lock_file_pkg
  solver_env
  stats_updater
  version_by_package_name
  opam_package
  ~pinned_package_names
  ~(candidates_cache : (Package_name.t, Context_for_dune.candidates) Table.t)
  =
  let name = Package_name.of_opam_package_name (OpamPackage.name opam_package) in
  let version =
    OpamPackage.version opam_package |> Package_version.of_opam_package_version
  in
  let resolved_package =
    (Table.find_exn candidates_cache name).resolved
    |> OpamPackage.Version.Map.find (Package_version.to_opam_package_version version)
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
      Package_name.Set.mem pinned_package_names name
      ||
      match url with
      | None -> false
      | Some url -> List.is_empty (OpamFile.URL.checksum url)
    in
    { Lock_dir.Pkg_info.name; version; dev; source; extra_sources }
  in
  let depends =
    let resolve what =
      Resolve_opam_formula.filtered_formula_to_package_names
        ~with_test:false
        (add_self_to_filter_env opam_package (Solver_env.to_env solver_env))
        version_by_package_name
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
    depends @ depopts |> List.map ~f:(fun package_name -> Loc.none, package_name)
  in
  let build_env action =
    let env_update =
      OpamFile.OPAM.build_env opam_file |> List.map ~f:opam_env_update_to_env_update
    in
    match env_update with
    | [] -> action
    | env_update -> Action.Withenv (env_update, action)
  in
  let get_solver_var variable_name =
    Solver_stats.Updater.expand_variable stats_updater variable_name;
    Solver_env.get solver_env variable_name
  in
  let build_command =
    if Resolved_package.dune_build resolved_package
    then Some Lock_dir.Build_command.Dune
    else (
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
        opam_commands_to_actions
          get_solver_var
          loc
          opam_package
          (OpamFile.OPAM.build opam_file)
      in
      List.concat [ subst_step; patch_step; build_step ]
      |> make_action
      |> Option.map ~f:build_env
      |> Option.map ~f:(fun action -> Lock_dir.Build_command.Action action))
  in
  let depexts =
    OpamFile.OPAM.depexts opam_file
    |> List.concat_map ~f:(fun (sys_pkgs, filter) ->
      let env = Solver_env.to_env solver_env in
      if OpamFilter.eval_to_bool ~default:false env filter
      then OpamSysPkg.Set.to_list_map OpamSysPkg.to_string sys_pkgs
      else [])
  in
  let install_command =
    OpamFile.OPAM.install opam_file
    |> opam_commands_to_actions get_solver_var loc opam_package
    |> make_action
    |> Option.map ~f:build_env
  in
  let exported_env =
    OpamFile.OPAM.env opam_file |> List.map ~f:opam_env_update_to_env_update
  in
  let kind = if opam_file_is_compiler opam_file then `Compiler else `Non_compiler in
  ( kind
  , { Lock_dir.Pkg.build_command; install_command; depends; depexts; info; exported_env }
  )
;;

let solve_package_list packages ~context =
  Fiber.collect_errors (fun () ->
    (* [Solver.solve] returns [Error] when it's unable to find a solution to
       the dependencies, but can also raise exceptions, for example if opam
       is unable to parse an opam file in the package repository. To prevent
       an unexpected opam exception from crashing dune, we catch all
       exceptions raised by the solver and report them as [User_error]s
       instead. *)
    Solver.solve context packages)
  >>| (function
         | Ok (Ok res) -> Ok res
         | Ok (Error e) -> Error (`Diagnostics e)
         | Error [] -> assert false
         | Error (exn :: _) ->
           (* CR-rgrinberg: this needs to be handled right *)
           Error (`Exn exn.exn))
  >>= function
  | Ok packages -> Fiber.return @@ Ok (Solver.packages_of_result packages)
  | Error (`Diagnostics e) ->
    let+ diagnostics = Solver.diagnostics e in
    Error (`Diagnostic_message diagnostics)
  | Error (`Exn exn) ->
    (match exn with
     | OpamPp.(Bad_format _ | Bad_format_list _ | Bad_version _) as bad_format ->
       (* CR-rgrinberg: needs to include locations *)
       User_error.raise [ Pp.text (OpamPp.string_of_bad_format bad_format) ]
     | User_error.E _ -> reraise exn
     | unexpected_exn ->
       Code_error.raise
         "Unexpected exception raised while solving dependencies"
         [ "exception", Exn.to_dyn unexpected_exn ])
;;

module Solver_result = struct
  type t =
    { lock_dir : Lock_dir.t
    ; files : File_entry.t Package_name.Map.Multi.t
    ; pinned_packages : Package_name.Set.t
    }
end

let reject_unreachable_packages =
  let reachable deps_of_package ~roots =
    let seen = ref Package_name.Set.empty in
    let rec loop = function
      | [] -> ()
      | pkg :: rest ->
        if Package_name.Set.mem !seen pkg
        then loop rest
        else (
          seen := Package_name.Set.add !seen pkg;
          let deps =
            match Package_name.Map.find deps_of_package pkg with
            | None -> []
            | Some deps -> deps
          in
          loop (List.rev_append deps rest))
    in
    loop roots;
    !seen
  in
  fun solver_env ~dune_version ~local_packages ~pkgs_by_name ->
    let roots = Package_name.Map.keys local_packages in
    let pkgs_by_version =
      Package_name.Map.merge pkgs_by_name local_packages ~f:(fun name lhs rhs ->
        match lhs, rhs with
        | None, None -> assert false
        | Some _, Some _ ->
          Code_error.raise
            "package is both local and returned by solver"
            [ "name", Package_name.to_dyn name ]
        | Some (lock_dir_pkg : Lock_dir.Pkg.t), None -> Some lock_dir_pkg.info.version
        | None, Some _pkg ->
          let version = Package_version.of_string "dev" in
          Some version)
    in
    let pkgs_by_name =
      Package_name.Map.merge pkgs_by_name local_packages ~f:(fun name lhs rhs ->
        match lhs, rhs with
        | None, None -> assert false
        | Some _, Some _ ->
          Code_error.raise
            "package is both local and returned by solver"
            [ "name", Package_name.to_dyn name ]
        | Some (pkg : Lock_dir.Pkg.t), None -> Some (List.map pkg.depends ~f:snd)
        | None, Some (pkg : Local_package.For_solver.t) ->
          let formula = Dependency_formula.to_filtered_formula pkg.dependencies in
          (* Use `dev` because at this point we don't have any version *)
          let opam_package =
            OpamPackage.of_string (sprintf "%s.dev" (Package_name.to_string pkg.name))
          in
          let env = add_self_to_filter_env opam_package (Solver_env.to_env solver_env) in
          let resolved =
            Resolve_opam_formula.filtered_formula_to_package_names
              env
              ~with_test:true
              (Package_name.Map.set pkgs_by_version Dune_dep.name dune_version)
              formula
          in
          let deps =
            match resolved with
            | Ok { regular; post = _ (* discard post deps *) } ->
              (* remove Dune from the formula as we remove it from solutions *)
              List.filter regular ~f:(fun pkg ->
                not (Package_name.equal Dune_dep.name pkg))
            | Error _ ->
              Code_error.raise
                "can't find a valid solution for the dependencies"
                [ "name", Package_name.to_dyn pkg.name ]
          in
          let depopts =
            List.filter_map pkg.depopts ~f:(fun (d : Package_dependency.t) ->
              Option.some_if
                (Package_name.Map.mem local_packages d.name
                 || Package_name.Map.mem pkgs_by_name d.name)
                d.name)
          in
          Some (deps @ depopts))
    in
    reachable pkgs_by_name ~roots
;;

let solve_lock_dir
  solver_env
  version_preference
  repos
  ~local_packages
  ~pins:pinned_packages
  ~constraints
  =
  let pinned_package_names = Package_name.Set.of_keys pinned_packages in
  let stats_updater = Solver_stats.Updater.init () in
  let context =
    Context_for_dune.create
      ~pinned_packages
      ~solver_env
      ~repos
      ~version_preference
      ~local_packages:
        (Package_name.Map.map local_packages ~f:Local_package.For_solver.to_opam_file)
      ~stats_updater
      ~constraints
  in
  Package_name.Map.to_list_map local_packages ~f:(fun name _ ->
    Package_name.to_opam_package_name name)
  |> solve_package_list ~context
  >>= function
  | Error _ as e -> Fiber.return e
  | Ok solution ->
    (* don't include local packages or dune in the lock dir *)
    let opam_packages_to_lock =
      let is_local_package = Package_name.Map.mem local_packages in
      List.filter solution ~f:(fun package ->
        let name = OpamPackage.name package |> Package_name.of_opam_package_name in
        (not (is_local_package name)) && not (Package_name.equal Dune_dep.name name))
    in
    let* candidates_cache = Fiber_cache.to_table context.candidates_cache in
    let ocaml, pkgs =
      let pkgs =
        let version_by_package_name =
          List.map solution ~f:(fun (package : OpamPackage.t) ->
            ( Package_name.of_opam_package_name (OpamPackage.name package)
            , Package_version.of_opam_package_version (OpamPackage.version package) ))
          |> Package_name.Map.of_list_exn
        in
        List.map opam_packages_to_lock ~f:(fun opam_package ->
          opam_package_to_lock_file_pkg
            solver_env
            stats_updater
            version_by_package_name
            opam_package
            ~pinned_package_names
            ~candidates_cache)
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
            (* CR-rgrinberg: needs to include locations *)
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
        let expanded_solver_variable_bindings =
          let stats = Solver_stats.Updater.snapshot stats_updater in
          Solver_stats.Expanded_variable_bindings.of_variable_set
            stats.expanded_variables
            solver_env
        in
        Package_name.Map.iter
          pkgs_by_name
          ~f:(fun { Lock_dir.Pkg.depends; info = { name; _ }; _ } ->
            List.iter depends ~f:(fun (loc, dep_name) ->
              if Package_name.Map.mem local_packages dep_name
              then
                User_error.raise
                  ~loc
                  [ Pp.textf
                      "Dune does not support packages outside the workspace depending on \
                       packages in the workspace. The package %S is not in the workspace \
                       but it depends on the package %S which is in the workspace."
                      (Package_name.to_string name)
                      (Package_name.to_string dep_name)
                  ]));
        let reachable =
          reject_unreachable_packages
            solver_env
            ~dune_version:(Package_version.of_opam_package_version context.dune_version)
            ~local_packages
            ~pkgs_by_name
        in
        let pkgs_by_name =
          Package_name.Map.filteri pkgs_by_name ~f:(fun name _ ->
            Package_name.Set.mem reachable name)
        in
        Lock_dir.create_latest_version
          pkgs_by_name
          ~local_packages:(Package_name.Map.values local_packages)
          ~ocaml
          ~repos:(Some repos)
          ~expanded_solver_variable_bindings
    in
    let+ files =
      let resolved_packages =
        List.map opam_packages_to_lock ~f:(fun opam_package ->
          let candidates =
            OpamPackage.name opam_package
            |> Package_name.of_opam_package_name
            |> Table.find_exn candidates_cache
          in
          OpamPackage.Version.Map.find
            (OpamPackage.version opam_package)
            candidates.resolved)
      in
      Resolved_package.get_opam_package_files resolved_packages
      >>| List.map2 resolved_packages ~f:(fun resolved_package entries ->
        let package_name =
          Resolved_package.package resolved_package
          |> OpamPackage.name
          |> Package_name.of_opam_package_name
        in
        package_name, entries)
      >>| List.filter ~f:(fun (_, entries) -> List.is_non_empty entries)
      >>| Package_name.Map.of_list_exn
    in
    Ok { Solver_result.lock_dir; files; pinned_packages = pinned_package_names }
;;
