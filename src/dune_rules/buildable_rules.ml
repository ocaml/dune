open Import
open Memo.O

let ocaml_flags t ~dir (spec : Dune_lang.Ocaml_flags.Spec.t) =
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

let with_lib_deps (t : Context.t) merlin_ident ~dir ~f =
  let prefix =
    if Context.merlin t
    then
      Merlin_ident.merlin_file_path dir merlin_ident
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
  | Parameter of Buildable.t * Lib_name.Local.t
  | Melange of
      { preprocess : Preprocess.With_instrumentation.t Preprocess.Per_module.t
      ; preprocessor_deps : Dep_conf.t list
      ; lint : Preprocess.Without_instrumentation.t Preprocess.Per_module.t
      ; empty_module_interface_if_absent : bool
      }

let fold_resolve (t : _ Preprocess.t) ~init ~f =
  match t with
  | Pps t -> Resolve.Memo.List.fold_left t.pps ~init ~f
  | No_preprocessing | Action _ | Future_syntax _ -> Resolve.Memo.return init
;;

let instrumentation_deps t ~instrumentation_backend =
  let open Resolve.Memo.O in
  let f = function
    | Preprocess.With_instrumentation.Ordinary _ -> Resolve.Memo.return []
    | Instrumentation_backend { libname; deps; flags = _ } ->
      instrumentation_backend libname
      >>| (function
       | Some _ -> deps
       | None -> [])
  in
  Instrumentation.fold t ~init:[] ~f:(fun t init ->
    let f acc t =
      let+ x = f t in
      x :: acc
    in
    fold_resolve t ~init ~f)
  >>| List.rev
  >>| List.flatten
;;

let modules_rules
      ~preprocess
      ~preprocessor_deps
      ~lint
      ~empty_module_interface_if_absent
      ~ctypes
      ~modules_loc
      ~buildable_loc
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
      Instrumentation.with_instrumentation preprocess ~instrumentation_backend
      |> Resolve.Memo.read_memo
    in
    let* instrumentation_deps =
      (* TODO wrong and blocks loading all the rules in this directory *)
      instrumentation_deps preprocess ~instrumentation_backend |> Resolve.Memo.read_memo
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
  let* () =
    match ctypes with
    | Some ctypes ->
      let (ctypes : Ctypes_field.t) = ctypes in
      let modules = Modules.With_vlib.modules modules in
      (* Here we collect all the modules that ctypes expects to be present in
         that stanza in order to validate their existence. We do this using
         [Memo.parallel_iter] in order to collect all the errors rather than
         just the first occurances. *)
      (ctypes.type_description.functor_loc, ctypes.type_description.functor_)
      :: List.map
           ~f:(fun (x : Ctypes_field.Function_description.t) -> x.functor_loc, x.functor_)
           ctypes.function_description
      |> Memo.parallel_iter ~f:(fun ((functor_loc, m) : Loc.t * Module_name.t) ->
        match Modules.With_vlib.find modules m with
        | Some _ -> Memo.return ()
        | None ->
          let loc =
            Option.first_some modules_loc buildable_loc
            |> Option.value
                 ~default:
                   (Path.build dir |> Path.drop_optional_build_context |> Loc.in_dir)
          in
          User_error.raise
            ~loc
            [ Pp.textf
                "Module %s is required by ctypes at %s but is missing in the modules \
                 field of the stanza."
                (Module_name.to_string m)
                (Loc.to_file_colon_line functor_loc)
            ])
    | None -> Memo.return ()
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
  let* () =
    match kind with
    | Executables _ | Library _ | Melange _ -> Memo.return ()
    | Parameter _ ->
      let* ocaml = Super_context.context sctx |> Context.ocaml in
      if Ocaml_config.parameterised_modules ocaml.ocaml_config
      then Memo.return ()
      else
        User_error.raise
          [ Pp.text "The compiler you are using is not compatible with library parameter"
          ]
  in
  let ( preprocess
      , preprocessor_deps
      , lint
      , empty_module_interface_if_absent
      , ctypes
      , modules_loc
      , buildable_loc )
    =
    match kind with
    | Executables (buildable, _) | Library (buildable, _) | Parameter (buildable, _) ->
      ( buildable.preprocess
      , buildable.preprocessor_deps
      , buildable.lint
      , buildable.empty_module_interface_if_absent
      , buildable.ctypes
      , Ordered_set_lang.Unexpanded.loc buildable.modules.modules
      , Some buildable.loc )
    | Melange { preprocess; preprocessor_deps; lint; empty_module_interface_if_absent } ->
      ( preprocess
      , preprocessor_deps
      , lint
      , empty_module_interface_if_absent
      , None
      , None
      , None )
  in
  let lib_name =
    match kind with
    | Executables _ | Melange _ -> None
    | Library (_, name) | Parameter (_, name) -> Some name
  in
  let empty_intf_modules =
    match kind with
    | Executables (_, modules) -> Some modules
    | Library _ | Melange _ | Parameter _ -> None
  in
  modules_rules
    ~preprocess
    ~preprocessor_deps
    ~lint
    ~empty_module_interface_if_absent
    ~ctypes
    ~modules_loc
    ~buildable_loc
    sctx
    expander
    ~dir
    scope
    modules
    ~lib_name
    ~empty_intf_modules
;;
