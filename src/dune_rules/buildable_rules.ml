open Import
open Memo.O

let ocaml_flags t ~dir (spec : Ocaml_flags.Spec.t) =
  let* expander = Super_context.expander t ~dir in
  let* flags =
    let+ ocaml_flags = Ocaml_flags_db.ocaml_flags_env ~dir in
    Ocaml_flags.make
      ~spec
      ~default:ocaml_flags
      ~eval:(Expander.expand_and_eval_set expander)
  in
  Source_tree.is_vendored (Path.Build.drop_build_context_exn dir)
  >>= function
  | false -> Memo.return flags
  | true ->
    let+ ocaml_version =
      let+ ocaml = Super_context.context t |> Context.ocaml in
      ocaml.version
    in
    Ocaml_flags.with_vendored_flags ~ocaml_version flags
;;

let gen_select_rules sctx ~dir compile_info =
  let open Memo.O in
  Lib.Compile.resolved_selects compile_info
  |> Resolve.Memo.read_memo
  >>= Memo.parallel_iter ~f:(fun { Lib.Compile.Resolved_select.dst_fn; src_fn } ->
    let dst = Path.Build.relative dir dst_fn in
    Super_context.add_rule
      sctx
      ~dir
      (Action_builder.with_file_targets
         ~file_targets:[ dst ]
         (let open Action_builder.O in
          let* src_fn = Resolve.read src_fn in
          let src = Path.build (Path.Build.relative dir src_fn) in
          let+ () = Action_builder.path src in
          let context = Super_context.context sctx in
          Action.Full.make (Copy_line_directive.action context ~src ~dst))))
;;

let with_lib_deps (t : Context.t) compile_info ~dir ~f =
  let prefix =
    if Context.merlin t
    then
      Lib.Compile.merlin_ident compile_info
      |> Merlin_ident.merlin_file_path dir
      |> Path.build
      |> Action_builder.path
      |> Action_builder.goal
    else Action_builder.return ()
  in
  Rules.prefix_rules prefix ~f
;;

type kind =
  | Executables of Buildable.t * (Loc.t * string) list
  | Library of Buildable.t * Lib_name.Local.t
  | Melange of
      { preprocess : Preprocess.With_instrumentation.t Preprocess.Per_module.t
      ; preprocessor_deps : Dep_conf.t list
      ; lint : Preprocess.Without_instrumentation.t Preprocess.Per_module.t
      ; empty_module_interface_if_absent : bool
      }

let modules_rules
  ~preprocess
  ~preprocessor_deps
  ~lint
  ~empty_module_interface_if_absent
  sctx
  expander
  ~dir
  scope
  modules
  ~lib_name
  ~empty_intf_modules
  =
  let* pp =
    let instrumentation_backend = Lib.DB.instrumentation_backend (Scope.libs scope) in
    let* preprocess_with_instrumentation =
      (* TODO wrong and blocks loading all the rules in this directory *)
      Resolve.Memo.read_memo
        (Preprocess.Per_module.with_instrumentation preprocess ~instrumentation_backend)
    in
    let* instrumentation_deps =
      (* TODO wrong and blocks loading all the rules in this directory *)
      Resolve.Memo.read_memo
        (Preprocess.Per_module.instrumentation_deps preprocess ~instrumentation_backend)
    in
    Pp_spec_rules.make
      sctx
      ~dir
      ~scope
      ~preprocess:preprocess_with_instrumentation
      ~expander
      ~preprocessor_deps
      ~instrumentation_deps
      ~lint
      ~lib_name
  in
  let add_empty_intf =
    let default = empty_module_interface_if_absent in
    match empty_intf_modules with
    | None -> fun _ -> default
    | Some mains ->
      if Dune_project.executables_implicit_empty_intf (Scope.project scope)
      then (
        let executable_names = List.map mains ~f:Module_name.of_string_allow_invalid in
        fun name -> default || List.mem executable_names name ~equal:Module_name.equal)
      else fun _ -> default
  in
  let+ modules =
    Modules.map_user_written modules ~f:(fun m ->
      let* m = Pp_spec.pp_module pp m in
      if add_empty_intf (Module.name m) && not (Module.has m ~ml_kind:Intf)
      then Module_compilation.with_empty_intf ~sctx ~dir m
      else Memo.return m)
  in
  modules, pp
;;

let modules_rules sctx kind expander ~dir scope modules =
  let preprocess, preprocessor_deps, lint, empty_module_interface_if_absent =
    match kind with
    | Executables (buildable, _) | Library (buildable, _) ->
      ( buildable.preprocess
      , buildable.preprocessor_deps
      , buildable.lint
      , buildable.empty_module_interface_if_absent )
    | Melange { preprocess; preprocessor_deps; lint; empty_module_interface_if_absent } ->
      preprocess, preprocessor_deps, lint, empty_module_interface_if_absent
  in
  let lib_name =
    match kind with
    | Executables _ | Melange _ -> None
    | Library (_, name) -> Some name
  in
  let empty_intf_modules =
    match kind with
    | Executables (_, modules) -> Some modules
    | Library _ | Melange _ -> None
  in
  modules_rules
    ~preprocess
    ~preprocessor_deps
    ~lint
    ~empty_module_interface_if_absent
    sctx
    expander
    ~dir
    scope
    modules
    ~lib_name
    ~empty_intf_modules
;;
