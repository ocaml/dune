open Import
open Fiber.O

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
  type filtered_formula = OpamTypes.filtered_formula
  type filter = OpamTypes.filter

  let local_package_default_version =
    OpamPackage.Version.of_string Lock_dir.Pkg_info.default_version
  ;;

  type t =
    { repos : Opam_repo.t list
    ; version_preference : Version_preference.t
    ; local_packages : OpamFile.OPAM.t OpamPackage.Name.Map.t
    ; solver_env : Solver_env.t
    ; dune_version : OpamPackage.Version.t
    ; stats_updater : Solver_stats.Updater.t
    }

  let create ~solver_env ~repos ~local_packages ~version_preference ~stats_updater =
    let dune_version =
      let major, minor = Dune_lang.Stanza.latest_version in
      OpamPackage.Version.of_string @@ sprintf "%d.%d" major minor
    in
    { repos; version_preference; local_packages; solver_env; dune_version; stats_updater }
  ;;

  type rejection = Unavailable

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

  (* Substitute variables with their values *)
  let resolve_solver_env
    (solver_env : Solver_env.t)
    (stats_updater : Solver_stats.Updater.t)
    : OpamTypes.filter -> OpamTypes.filter
    =
    OpamFilter.map_up (function
      | FIdent ([], variable, None) as filter ->
        (match Solver_env.Variable.of_string_opt (OpamVariable.to_string variable) with
         | None -> filter
         | Some variable ->
           Solver_stats.Updater.expand_variable stats_updater variable;
           (match Solver_env.get solver_env variable with
            | Unset_sys -> filter
            | String string -> FString string))
      | other -> other)
  ;;

  let is_opam_available =
    (* The solver can call this function several times on the same package. If
       the package contains an invalid `available` filter we want to print a
       warning, but only once per package. This variable will keep track of the
       packages for which we've printed a warning. *)
    let warned_packages = ref OpamPackage.Set.empty in
    fun t opam ->
      let available = OpamFile.OPAM.available opam in
      match
        let available_vars_resolved =
          resolve_solver_env t.solver_env t.stats_updater available
        in
        eval_to_bool available_vars_resolved
      with
      | Ok available -> available
      | Error error ->
        let package = OpamFile.OPAM.package opam in
        if not (OpamPackage.Set.mem package !warned_packages)
        then (
          warned_packages := OpamPackage.Set.add package !warned_packages;
          match error with
          | `Not_a_bool msg ->
            let package_string = OpamFile.OPAM.package opam |> OpamPackage.to_string in
            let available_string = OpamFilter.to_string available in
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
      Fiber.return [ version, Ok opam_file ]
    | None ->
      let+ opam_files = Opam_repo.load_all_versions t.repos name in
      (* The CONTEXT interface doesn't give us a way to report this type of
         error and there's not enough context to give a helpful error message
         so just tell opam_0install that there are no versions of this
         package available (technically true) and let it produce the error
         message. *)
      let opam_files_in_priority_order =
        List.sort opam_files ~compare:(opam_version_compare t)
      in
      List.map opam_files_in_priority_order ~f:(fun opam_file ->
        let opam_file_result =
          if is_opam_available t opam_file then Ok opam_file else Error Unavailable
        in
        OpamFile.OPAM.version opam_file, opam_file_result)
  ;;

  let user_restrictions : t -> OpamPackage.Name.t -> OpamFormula.version_constraint option
    =
    let dune = OpamPackage.Name.of_string "dune" in
    fun t pkg ->
      if OpamPackage.Name.equal dune pkg then Some (`Eq, t.dune_version) else None
  ;;

  let map_filters ~(f : filter -> filter) : filtered_formula -> filtered_formula =
    OpamFilter.gen_filter_formula
      (OpamFormula.partial_eval (function
        | OpamTypes.Filter flt -> `Formula (Atom (OpamTypes.Filter (f flt)))
        | Constraint _ as constraint_ -> `Formula (Atom constraint_)))
  ;;

  let filter_deps t package filtered_formula =
    let package_is_local =
      OpamPackage.Name.Map.mem (OpamPackage.name package) t.local_packages
    in
    map_filters filtered_formula ~f:(resolve_solver_env t.solver_env t.stats_updater)
    |> OpamFilter.filter_deps
         ~build:true
         ~post:false
         ~dev:false
         ~default:false
         ~test:package_is_local
         ~doc:false
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
      match package_name with
      | None ->
        Package_variable.(
          to_pform
            { name = Package_variable.Name.of_string variable_string; scope = Self })
      | Some package_name ->
        Package_variable.(
          to_pform
            { name = Package_variable.Name.of_string variable_string
            ; scope =
                Package (Package_name.of_string (OpamPackage.Name.to_string package_name))
            })
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

let opam_file_map_of_dune_package_map dune_package_map =
  Package_name.Map.to_list_map dune_package_map ~f:(fun dune_package_name opam_file ->
    let opam_package_name =
      Package_name.to_string dune_package_name |> OpamPackage.Name.of_string
    in
    opam_package_name, opam_file)
  |> OpamPackage.Name.Map.of_list
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
  context_for_dune
  ~all_package_names
  ~repos
  ~local_packages
  ~experimental_translate_opam_filters
  opam_package
  =
  let name = OpamPackage.name opam_package in
  let version = OpamPackage.version opam_package |> OpamPackage.Version.to_string in
  let dev = OpamPackage.Name.Map.mem name local_packages in
  let+ opam_file, loc =
    let+ { Opam_repo.With_file.opam_file = opam_file_with_filters; file } =
      match OpamPackage.Name.Map.find_opt name local_packages with
      | Some local_package -> Fiber.return local_package
      | None ->
        let+ opam_files =
          let+ pkgs =
            Fiber.parallel_map repos ~f:(fun repo ->
              Opam_repo.load_opam_package repo opam_package)
          in
          List.filter_opt pkgs
        in
        (match opam_files with
         | [ opam_file ] -> opam_file
         | _ -> Code_error.raise "Couldn't map opam package to a repository" [])
    in
    let opam_file =
      if experimental_translate_opam_filters
      then opam_file_with_filters
      else remove_filters_from_opam_file opam_file_with_filters
    in
    let loc = Loc.in_file file in
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
    { Lock_dir.Pkg_info.name = Package_name.of_string (OpamPackage.Name.to_string name)
    ; version
    ; dev
    ; source
    ; extra_sources
    }
  in
  (* This will collect all the atoms from the package's dependency formula regardless of conditions *)
  let deps =
    Context_for_dune.filter_deps context_for_dune opam_package opam_file.depends
    |> OpamFormula.fold_right
         (fun acc (name, _condition) ->
           if OpamPackage.Name.Set.mem name all_package_names then name :: acc else acc)
         []
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

let solve_package_list local_package_names context =
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
  let local_packages = opam_file_map_of_dune_package_map local_packages in
  let is_local_package package =
    OpamPackage.Name.Map.mem (OpamPackage.name package) local_packages
  in
  let stats_updater = Solver_stats.Updater.init () in
  let context =
    let local_packages =
      OpamPackage.Name.Map.map
        (fun (w : Opam_repo.With_file.t) -> w.opam_file)
        local_packages
    in
    Context_for_dune.create
      ~solver_env
      ~repos
      ~version_preference
      ~local_packages
      ~stats_updater
  in
  let* solver_result =
    let+ solution =
      solve_package_list (OpamPackage.Name.Map.keys local_packages) context
    in
    Result.map solution ~f:(fun solution ->
      (* don't include local packages in the lock dir *)
      let all_package_names =
        List.map solution ~f:OpamPackage.name |> OpamPackage.Name.Set.of_list
      in
      let opam_packages_to_lock = List.filter solution ~f:(Fun.negate is_local_package) in
      let* ocaml, pkgs =
        let+ pkgs =
          Fiber.parallel_map opam_packages_to_lock ~f:(fun opam_package ->
            opam_package_to_lock_file_pkg
              context
              ~all_package_names
              ~repos
              ~local_packages
              ~experimental_translate_opam_filters
              opam_package)
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
            ~ocaml
            ~repos:(Some repos)
            ~expanded_solver_variable_bindings
      in
      let+ files =
        let+ file_entry_candidates =
          Fiber.parallel_map opam_packages_to_lock ~f:(fun opam_package ->
            let+ entries_per_repo =
              Fiber.parallel_map repos ~f:(fun repo ->
                let+ file_entries = Opam_repo.get_opam_package_files repo opam_package in
                match file_entries with
                | [] -> None
                | file_entries -> Some file_entries)
            in
            let first_matching_repo =
              List.find_map entries_per_repo ~f:(function
                | None -> None
                | Some [] -> None
                | Some entries -> Some entries)
            in
            match first_matching_repo with
            | None -> None
            | Some files ->
              Some
                ( Package_name.of_string
                    (OpamPackage.Name.to_string (OpamPackage.name opam_package))
                , files ))
        in
        file_entry_candidates |> List.filter_opt |> Package_name.Map.of_list_exn
      in
      { Solver_result.lock_dir; files })
  in
  match solver_result with
  | Ok ok ->
    let+ ok = ok in
    Ok ok
  | Error _ as e -> Fiber.return e
;;
