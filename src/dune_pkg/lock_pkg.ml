open Import

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

(* Handles the special case for packages whose names contain '+' characters
   where a special form of string interpolation is used. From the opam manual:
   Warning: if the package name contains a + character (e.g. conf-g++), their
   variables may only be accessed using opam 2.2 via string interpolation,
   with the following syntax:

     "%{?conf-g++:your-variable:}%"
*)
let desugar_special_string_interpolation_syntax
      ((packages, variable, string_converter) as fident)
  =
  match string_converter with
  | Some (package_and_variable, "")
    when List.is_empty packages && String.is_empty (OpamVariable.to_string variable) ->
    (match String.lsplit2 package_and_variable ~on:':' with
     | Some (package, variable) ->
       ( [ Some (OpamPackage.Name.of_string package) ]
       , OpamVariable.of_string variable
       , None )
     | None -> fident)
  | _ -> fident
;;

let opam_fident_to_slang ~loc fident =
  let packages, variable, string_converter =
    OpamFilter.desugar_fident fident |> desugar_special_string_interpolation_syntax
  in
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

let opam_env_update_to_env_update (var, env_op, value_string, _) : _ Action.Env_update.t =
  { Action.Env_update.op = env_op
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
  let filter_to_slang (filter : OpamTypes.filter) =
    match filter with
    | FString s -> opam_string_to_slang ~package ~loc s
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
  let rec filter_to_blang (filter : OpamTypes.filter) =
    match filter with
    | FBool true -> Blang.Ast.true_
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
        List.filter_map args ~f:(fun ((simple_arg : OpamTypes.simple_arg), filter) ->
          let filter = Option.map filter ~f:(simplify_filter get_solver_var) in
          match partial_eval_filter filter with
          | `Skip -> None
          | `Filter filter ->
            let slang =
              let slang =
                match simple_arg with
                | CString s -> opam_string_to_slang ~package ~loc s
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
                    let filter_blang_handling_undefined =
                      (* Wrap the blang filter so that if any undefined
                         variables are expanded while evaluating the filter,
                         the filter will return false. *)
                      let slang =
                        Slang.catch_undefined_var
                          (Slang.blang filter_blang)
                          ~fallback:(Slang.bool false)
                      in
                      Blang.Expr slang
                    in
                    Slang.when_ filter_blang_handling_undefined slang)))
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

let opam_package_to_lock_file_pkg
      solver_env
      stats_updater
      version_by_package_name
      opam_package
      ~pinned_package_names
      ~candidates_cache
  =
  let name = Package_name.of_opam_package_name (OpamPackage.name opam_package) in
  let version =
    OpamPackage.version opam_package |> Package_version.of_opam_package_version
  in
  let resolved_package =
    candidates_cache name
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
    let avoid = List.mem opam_file.flags Pkgflag_AvoidVersion ~equal:Poly.equal in
    { Lock_dir.Pkg_info.name; version; dev; avoid; source; extra_sources }
  in
  let depends =
    let resolve what =
      Resolve_opam_formula.filtered_formula_to_package_names
        ~with_test:false
        ~packages:version_by_package_name
        ~env:(add_self_to_filter_env opam_package (Solver_env.to_env solver_env))
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
