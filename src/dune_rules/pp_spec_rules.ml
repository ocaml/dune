open Import
open Memo.O

let pped_module m ~f =
  let open Memo.O in
  let pped = Module.pped m in
  let+ () =
    Module.iter m ~f:(fun ml_kind file ->
      let pp_path =
        Module.file pped ~ml_kind |> Option.value_exn |> Path.as_in_build_dir_exn
      in
      let file = Path.as_in_build_dir_exn (Module.File.path file) in
      f ml_kind file pp_path)
  in
  pped
;;

let get_rules sctx key =
  let ctx = Super_context.context sctx in
  let build_context = Context.build_context ctx in
  let exe = Ppx_driver.ppx_exe_path build_context ~key in
  let* pp_names, scope =
    match Digest.from_hex key with
    | None ->
      User_error.raise
        [ Pp.textf "invalid ppx key for %s" (Path.Build.to_string_maybe_quoted exe) ]
    | Some key ->
      let { Ppx_driver.Key.Decoded.pps; project_root } = Ppx_driver.Key.decode key in
      let+ scope =
        let dir =
          match project_root with
          | None -> Context.build_dir ctx
          | Some dir -> Path.Build.append_source build_context.build_dir dir
        in
        Scope.DB.find_by_dir dir
      in
      pps, scope
  in
  let open Memo.O in
  let* pps =
    let lib_db = Scope.libs scope in
    List.map pp_names ~f:(fun x -> Loc.none, x) |> Lib.DB.resolve_pps lib_db
  in
  Ppx_driver.build_ppx_driver sctx ~scope ~pps ~pp_names ~target:exe
;;

let gen_rules sctx components =
  match components with
  | [ key ] -> get_rules sctx key
  | _ -> Memo.return ()
;;

let promote_correction fn build ~suffix =
  let open Action_builder.O in
  let+ act = build in
  Action.Full.reduce
    [ act
    ; Action.Full.make
        (Action.diff
           ~optional:true
           (Path.build fn)
           (Path.Build.extend_basename fn ~suffix))
    ]
;;

let promote_correction_with_target fn build ~suffix =
  Action_builder.progn
    [ build
    ; Action_builder.with_no_targets
        (Action_builder.return
           (Action.Full.make
              (Action.diff
                 ~optional:true
                 (Path.build fn)
                 (Path.Build.extend_basename fn ~suffix))))
    ]
;;

let sandbox_of_setting = function
  | `Set_by_user d | `Default d -> d
;;

let action_for_pp ~sandbox ~loc ~expander ~action ~src =
  let expander =
    let bindings = Pform.Map.singleton (Var Input_file) [ Value.Path (Path.build src) ] in
    Expander.add_bindings expander ~bindings
  in
  let open Action_builder.O in
  Action_builder.path (Path.build src)
  >>> Action_unexpanded.expand_no_targets
        action
        ~chdir:(Expander.context expander |> Context_name.build_dir)
        ~loc
        ~expander
        ~deps:[]
        ~what:"preprocessing actions"
  >>| Action.Full.add_sandbox sandbox
;;

let action_for_pp_with_target ~sandbox ~loc ~expander ~action ~src ~target =
  let action = action_for_pp ~sandbox ~loc ~expander ~action ~src in
  Action_builder.with_stdout_to target action
;;

(* Generate rules for the dialect modules in [modules] and return a a new module
   with only OCaml sources *)
let setup_dialect_rules sctx ~sandbox ~dir ~expander (m : Module.t) =
  let open Memo.O in
  let ml = Module.ml_source m in
  let+ () =
    Module.iter m ~f:(fun ml_kind f ->
      Dialect.preprocess (Module.File.dialect f) ml_kind
      |> Memo.Option.iter ~f:(fun (loc, action) ->
        let src = Path.as_in_build_dir_exn (Module.File.path f) in
        let dst =
          Module.file ml ~ml_kind |> Option.value_exn |> Path.as_in_build_dir_exn
        in
        Super_context.add_rule
          sctx
          ~dir
          (action_for_pp_with_target ~sandbox ~loc ~expander ~action ~src ~target:dst)))
  in
  ml
;;

let add_corrected_suffix_binding expander suffix =
  let bindings = Pform.Map.singleton (Var Corrected_suffix) [ Value.String suffix ] in
  Expander.add_bindings expander ~bindings
;;

let driver_flags expander ~corrected_suffix ~driver_flags ~standard =
  let expander = add_corrected_suffix_binding expander corrected_suffix in
  Expander.expand_and_eval_set expander driver_flags ~standard
;;

let lint_module sctx ~sandbox ~dir ~expander ~lint ~lib_name ~scope =
  let open Action_builder.O in
  let add_alias build =
    Super_context.add_alias_action sctx (Alias.make Alias0.lint ~dir) build ~dir
  in
  let lint =
    Module_name.Per_item.map lint ~f:(function
      | Preprocess.No_preprocessing -> fun ~source:_ ~ast:_ -> Memo.return ()
      | Future_syntax loc ->
        User_error.raise ~loc [ Pp.text "'compat' cannot be used as a linter" ]
      | Action (loc, action) ->
        fun ~source ~ast:_ ->
          Module.iter source ~f:(fun _ (src : Module.File.t) ->
            let src = Path.as_in_build_dir_exn (Module.File.path src) in
            add_alias ~loc (action_for_pp ~sandbox ~loc ~expander ~action ~src))
      | Pps { loc; pps; flags; staged } ->
        if staged
        then
          User_error.raise
            ~loc
            [ Pp.text "Staged ppx rewriters cannot be used as linters." ];
        let corrected_suffix = ".lint-corrected" in
        let ctx = Super_context.context sctx in
        let driver_and_flags =
          Action_builder.memoize
            ~cutoff:
              (Tuple.T3.equal
                 Path.Build.equal
                 (List.equal String.equal)
                 (List.equal String.equal))
            "ppx driver and flags"
            (let* () = Action_builder.return () in
             let* exe, driver, flags =
               Ppx_driver.ppx_driver_and_flags
                 ctx
                 ~expander
                 ~loc
                 ~lib_name
                 ~flags
                 ~scope
                 pps
             in
             let+ ppx_flags =
               driver_flags
                 expander
                 ~corrected_suffix
                 ~driver_flags:(Ppx_driver.Driver.lint_flags driver)
                 ~standard:(Action_builder.return [])
             in
             exe, ppx_flags, flags)
        in
        fun ~source ~ast ->
          Module.iter ast ~f:(fun ml_kind src ->
            add_alias
              ~loc
              (promote_correction
                 ~suffix:corrected_suffix
                 (Path.as_in_build_dir_exn
                    (Option.value_exn (Module.file source ~ml_kind)))
                 (let* exe, flags, args = driver_and_flags in
                  let dir = ctx |> Context.build_dir |> Path.build in
                  Command.run'
                    ~dir
                    (Ok (Path.build exe))
                    [ As args
                    ; Command.Ml_kind.ppx_driver_flag ml_kind
                    ; Dep (Module.File.path src)
                    ; As flags
                    ]))))
  in
  Staged.stage
  @@ fun ~(source : Module.t) ~ast ->
  Module_name.Per_item.get lint (Module.name source) ~source ~ast
;;

let pp_one_module
  sctx
  ~lib_name
  ~scope
  ~preprocessor_deps
  ~(lint_module : source:_ -> ast:_ -> unit Memo.t)
  ~sandbox
  ~dir
  ~expander
  (pp : _ Preprocess.Without_future_syntax.t)
  =
  let open Action_builder.O in
  match pp with
  | No_preprocessing ->
    Staged.stage
    @@ fun m ~lint ->
    let open Memo.O in
    let* ast =
      let sandbox = sandbox_of_setting sandbox in
      setup_dialect_rules sctx ~sandbox ~dir ~expander m
    in
    let+ () = Memo.when_ lint (fun () -> lint_module ~ast ~source:m) in
    ast
  | Action (loc, action) ->
    Staged.stage
    @@ fun m ~lint ->
    let open Memo.O in
    let* ast =
      let sandbox = sandbox_of_setting sandbox in
      pped_module m ~f:(fun _kind src dst ->
        let action =
          action_for_pp_with_target ~sandbox ~loc ~expander ~action ~src ~target:dst
        in
        Super_context.add_rule
          sctx
          ~loc
          ~dir
          (let open Action_builder.With_targets.O in
           Action_builder.with_no_targets preprocessor_deps >>> action))
      >>= setup_dialect_rules sctx ~sandbox ~dir ~expander
    in
    let+ () = Memo.when_ lint (fun () -> lint_module ~ast ~source:m) in
    ast
  | Pps { loc; pps; flags; staged } ->
    Staged.stage
    @@
    if staged
    then (
      let dash_ppx_flag =
        let+ args =
          Action_builder.memoize
            ~cutoff:(List.equal String.equal)
            "ppx command"
            (let* exe, driver, flags =
               Ppx_driver.ppx_driver_and_flags
                 (Super_context.context sctx)
                 ~expander
                 ~loc
                 ~scope
                 ~flags
                 ~lib_name
                 pps
             in
             let* driver_flags =
               Expander.expand_and_eval_set
                 expander
                 (Ppx_driver.Driver.as_ppx_flags driver)
                 ~standard:(Action_builder.return [ "--as-ppx" ])
             and* () = preprocessor_deps in
             Command.expand_no_targets
               ~dir:(Super_context.context sctx |> Context.build_dir |> Path.build)
               (S [ Dep (Path.build exe); As driver_flags; As flags ]))
        in
        [ "-ppx"; String.quote_list_for_shell args ]
      in
      let pp =
        let sandbox =
          match sandbox with
          | `Set_by_user d -> d
          | `Default _ -> Sandbox_config.no_special_requirements
        in
        Some (dash_ppx_flag, sandbox)
      in
      let sandbox = sandbox_of_setting sandbox in
      fun m ~lint ->
        let open Memo.O in
        let* ast = setup_dialect_rules sctx ~sandbox ~dir ~expander m in
        let+ () = Memo.when_ lint (fun () -> lint_module ~ast ~source:m) in
        Module.set_pp ast pp)
    else (
      let corrected_suffix = ".ppx-corrected" in
      let driver_and_flags =
        Action_builder.memoize
          ~cutoff:
            (Tuple.T3.equal
               Path.Build.equal
               (List.equal String.equal)
               (List.equal String.equal))
          "ppx driver and flags"
          (let* () = Action_builder.return () in
           let* exe, driver, flags =
             Ppx_driver.ppx_driver_and_flags
               (Super_context.context sctx)
               ~expander
               ~loc
               ~lib_name
               ~flags
               ~scope
               pps
           in
           let+ ppx_flags =
             driver_flags
               expander
               ~corrected_suffix
               ~driver_flags:(Ppx_driver.Driver.flags driver)
               ~standard:(Action_builder.return [ "--as-ppx" ])
           in
           exe, ppx_flags, flags)
      in
      let sandbox = sandbox_of_setting sandbox in
      fun m ~lint ->
        let open Memo.O in
        let* ast = setup_dialect_rules sctx ~sandbox ~dir ~expander m in
        let* () = Memo.when_ lint (fun () -> lint_module ~ast ~source:m) in
        pped_module ast ~f:(fun ml_kind src dst ->
          Super_context.add_rule
            sctx
            ~loc
            ~dir
            (promote_correction_with_target
               ~suffix:corrected_suffix
               (Path.as_in_build_dir_exn (Option.value_exn (Module.file m ~ml_kind)))
               (Action_builder.with_file_targets
                  ~file_targets:[ dst ]
                  (let open Action_builder.O in
                   preprocessor_deps
                   >>> let* exe, flags, args = driver_and_flags in
                       let dir =
                         Super_context.context sctx |> Context.build_dir |> Path.build
                       in
                       Command.run'
                         ~dir
                         (Ok (Path.build exe))
                         [ As args
                         ; A "-o"
                         ; Path (Path.build dst)
                         ; Command.Ml_kind.ppx_driver_flag ml_kind
                         ; Dep (Path.build src)
                         ; Hidden_deps
                             (Module.source m ~ml_kind
                              |> Option.value_exn
                              |> Module.File.path
                              |> Dep.file
                              |> Dep.Set.singleton)
                         ; As flags
                         ]
                       >>| Action.Full.add_sandbox sandbox)))))
;;

let make
  sctx
  ~dir
  ~expander
  ~lint
  ~preprocess
  ~preprocessor_deps
  ~instrumentation_deps
  ~lib_name
  ~scope
  =
  let preprocessor_deps = preprocessor_deps @ instrumentation_deps in
  let+ ocaml = Context.ocaml (Super_context.context sctx) in
  let preprocess =
    Module_name.Per_item.map preprocess ~f:(fun pp ->
      Preprocess.remove_future_syntax ~for_:Compiler pp ocaml.version)
  in
  let preprocessor_deps, sandbox = Dep_conf_eval.unnamed preprocessor_deps ~expander in
  let sandbox =
    match Sandbox_config.equal Sandbox_config.no_special_requirements sandbox with
    | false -> `Set_by_user sandbox
    | true ->
      let project = Scope.project scope in
      let dune_version = Dune_project.dune_version project in
      `Default
        (if dune_version >= (3, 3) then Sandbox_config.needs_sandboxing else sandbox)
  in
  let preprocessor_deps = Action_builder.memoize "preprocessor deps" preprocessor_deps in
  let lint_module =
    let sandbox = sandbox_of_setting sandbox in
    Staged.unstage (lint_module sctx ~sandbox ~dir ~expander ~lint ~lib_name ~scope)
  in
  Module_name.Per_item.map preprocess ~f:(fun spec ->
    Staged.unstage
    @@ pp_one_module
         sctx
         ~lib_name
         ~scope
         ~preprocessor_deps
         ~lint_module
         ~sandbox
         ~dir
         ~expander
         spec)
  |> Pp_spec.make
;;
