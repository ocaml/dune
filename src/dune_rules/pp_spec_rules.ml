open Import
open Memo.O
module Key = Ppx_driver.Key

let pped_module m ~f =
  let open Memo.O in
  let pped = Module.pped m in
  let+ () =
    Module.iter m ~f:(fun ml_kind file ->
        let pp_path =
          Module.file pped ~ml_kind |> Option.value_exn
          |> Path.as_in_build_dir_exn
        in
        let file = Path.as_in_build_dir_exn (Module.File.path file) in
        f ml_kind file pp_path)
  in
  pped

let build_ppx_driver sctx ~scope ~target ~pps ~pp_names =
  let open Memo.O in
  let ctx = Super_context.context sctx in
  let* driver_and_libs =
    let ( let& ) t f = Resolve.Memo.bind t ~f in
    let& pps = Resolve.Memo.lift pps in
    let& pps = Lib.closure ~linking:true pps in
    Ppx_driver.Driver.select pps ~loc:(Dot_ppx (target, pp_names))
    >>| Resolve.map ~f:(fun driver -> (driver, pps))
    >>| (* Extend the dependency stack as we don't have locations at this
           point *)
    Resolve.push_stack_frame ~human_readable_description:(fun () ->
        Dyn.pp (List [ String "pps"; Dyn.(list Lib_name.to_dyn) pp_names ]))
  in
  (* CR-someday diml: what we should do is build the .cmx/.cmo once and for all
     at the point where the driver is defined. *)
  let dir = Path.Build.parent_exn target in
  let main_module_name =
    Module_name.of_string_allow_invalid (Loc.none, "_ppx")
  in
  let module_ = Module.generated ~kind:Impl ~src_dir:dir [ main_module_name ] in
  let ml_source =
    Module.file ~ml_kind:Impl module_
    |> Option.value_exn |> Path.as_in_build_dir_exn
  in
  let* () =
    Super_context.add_rule sctx ~dir
      (Action_builder.write_file_dyn ml_source
         (Resolve.read
            (let open Resolve.O in
            let+ driver_info =
              let+ driver, _ = driver_and_libs in
              Ppx_driver.Driver.info driver
            in
            sprintf "let () = %s ()\n" driver_info.main)))
  in
  let linkages = [ Exe.Linkage.native_or_custom ctx ] in
  let program : Exe.Program.t =
    { name = Filename.remove_extension (Path.Build.basename target)
    ; main_module_name
    ; loc = Loc.none
    }
  in
  let obj_dir = Obj_dir.for_pp ~dir in
  let* cctx =
    let* expander = Super_context.expander sctx ~dir in
    let requires_compile = Resolve.map driver_and_libs ~f:snd in
    let requires_link = Memo.lazy_ (fun () -> Memo.return requires_compile) in
    let flags = Ocaml_flags.of_list [ "-g"; "-w"; "-24" ] in
    let opaque = Compilation_context.Explicit false in
    let modules = Modules.singleton_exe module_ in
    Compilation_context.create ~super_context:sctx ~scope ~expander ~obj_dir
      ~modules ~flags
      ~requires_compile:(Memo.return requires_compile)
      ~requires_link ~opaque ~js_of_ocaml:None ~package:None ~bin_annot:false ()
  in
  let+ (_ : Exe.dep_graphs) =
    Exe.build_and_link ~program ~linkages cctx ~promote:None
  in
  ()

let get_rules sctx key =
  let ctx = Super_context.context sctx in
  let exe = Ppx_driver.ppx_exe ctx ~key in
  let* pp_names, scope =
    match Digest.from_hex key with
    | None ->
      User_error.raise
        [ Pp.textf "invalid ppx key for %s"
            (Path.Build.to_string_maybe_quoted exe)
        ]
    | Some key ->
      let { Key.Decoded.pps; project_root } = Key.decode key in
      let+ scope =
        let dir =
          match project_root with
          | None -> ctx.build_dir
          | Some dir -> Path.Build.append_source ctx.build_dir dir
        in
        Scope.DB.find_by_dir dir
      in
      (pps, scope)
  in
  let open Memo.O in
  let* pps =
    let lib_db = Scope.libs scope in
    List.map pp_names ~f:(fun x -> (Loc.none, x)) |> Lib.DB.resolve_pps lib_db
  in
  build_ppx_driver sctx ~scope ~pps ~pp_names ~target:exe

let gen_rules sctx components =
  match components with
  | [ key ] -> get_rules sctx key
  | _ -> Memo.return ()

let promote_correction fn build ~suffix =
  let open Action_builder.O in
  let+ act = build in
  Action.Full.reduce
    [ act
    ; Action.Full.make
        (Action.diff ~optional:true (Path.build fn)
           (Path.Build.extend_basename fn ~suffix))
    ]

let promote_correction_with_target fn build ~suffix =
  Action_builder.progn
    [ build
    ; Action_builder.with_no_targets
        (Action_builder.return
           (Action.Full.make
              (Action.diff ~optional:true (Path.build fn)
                 (Path.Build.extend_basename fn ~suffix))))
    ]

let sandbox_of_setting = function
  | `Set_by_user d | `Default d -> d

let action_for_pp ~dir ~sandbox ~loc ~expander ~action ~src ~lib_name ~ml_kind =
  let chdir = (Expander.context expander).build_dir in
  let expander =
    let bindings =
      Pform.Map.of_list_exn
        [ (Var Input_file, [ Value.Path (Path.build src) ]) ]
    in
    Expander.add_bindings expander ~bindings
  in
  let+ action =
    let+ action =
      let ml_kind = Some ml_kind in
      Action_unexpanded.expand action ~chdir ~loc ~expander ~deps:[] ~lib_name
        ~ml_kind ~targets_dir:dir ~targets:Targets_spec.Infer
        ~what:(User_action_without_targets { what = "preprocessing actions" })
    in

    Action_builder.With_targets.map_build action ~f:(fun action ->
        let open Action_builder.O in
        Action_builder.path (Path.build src) >>> action)
  in
  Action_builder.With_targets.map action ~f:(Action.Full.add_sandbox sandbox)

let action_for_pp_with_target ~dir ~sandbox ~loc ~expander ~action ~src ~target
    ~lib_name ~ml_kind =
  let+ action =
    action_for_pp ~dir ~sandbox ~loc ~expander ~action ~ml_kind ~src ~lib_name
  in
  Action_builder.With_targets.map action ~f:(fun build ->
      Action.Full.map build ~f:(Action.with_stdout_to target))
  |> Action_builder.With_targets.add ~file_targets:[ target ]

(* Generate rules for the dialect modules in [modules] and return a a new module
   with only OCaml sources *)
let setup_dialect_rules sctx ~sandbox ~dir ~expander ~lib_name (m : Module.t) =
  let open Memo.O in
  let ml = Module.ml_source m in
  let+ () =
    Module.iter m ~f:(fun ml_kind f ->
        Dialect.preprocess (Module.File.dialect f) ml_kind
        |> Memo.Option.iter ~f:(fun (loc, action) ->
               let src = Path.as_in_build_dir_exn (Module.File.path f) in
               let dst =
                 Module.file ml ~ml_kind |> Option.value_exn
                 |> Path.as_in_build_dir_exn
               in
               let* action =
                 action_for_pp_with_target ~dir ~sandbox ~loc ~expander ~action
                   ~ml_kind ~src ~target:dst ~lib_name
               in
               Super_context.add_rule sctx ~dir action))
  in
  ml

let lint_module sctx ~sandbox ~dir ~expander ~lint ~lib_name ~scope =
  let open Action_builder.O in
  let alias = Alias.lint ~dir in
  let add_alias build = Super_context.add_alias_action sctx alias build ~dir in
  let lint =
    Module_name.Per_item.map lint ~f:(function
      | Preprocess.No_preprocessing -> fun ~source:_ ~ast:_ -> Memo.return ()
      | Future_syntax loc ->
        User_error.raise ~loc [ Pp.text "'compat' cannot be used as a linter" ]
      | Action (loc, action) ->
        fun ~source ~ast:_ ->
          Module.iter source ~f:(fun ml_kind (src : Module.File.t) ->
              let open Memo.O in
              let src = Path.as_in_build_dir_exn (Module.File.path src) in
              let* action =
                action_for_pp ~dir ~sandbox ~loc ~expander ~action ~src ~ml_kind
                  ~lib_name
              in
              add_alias ~loc:(Some loc) action.build)
      | Pps { loc; pps; flags; staged } ->
        if staged then
          User_error.raise ~loc
            [ Pp.text "Staged ppx rewriters cannot be used as linters." ];
        let corrected_suffix = ".lint-corrected" in
        let driver_and_flags =
          Action_builder.memoize
            ~cutoff:
              (Tuple.T3.equal Path.Build.equal (List.equal String.equal)
                 (List.equal String.equal))
            "ppx driver and flags"
            (let* () = Action_builder.return () in
             let* exe, driver, flags =
               let context = Super_context.context sctx in
               Ppx_driver.ppx_exe_driver_and_flags ~context ~expander ~loc
                 ~lib_name ~flags ~scope pps
             in
             let+ ppx_flags =
               let driver_info = Ppx_driver.Driver.info driver in
               Ppx_driver.driver_flags expander ~corrected_suffix
                 ~driver_flags:driver_info.lint_flags
                 ~standard:(Action_builder.return [])
             in
             (exe, ppx_flags, flags))
        in
        fun ~source ~ast ->
          Module.iter ast ~f:(fun ml_kind src ->
              add_alias ~loc:(Some loc)
                (promote_correction ~suffix:corrected_suffix
                   (Path.as_in_build_dir_exn
                      (Option.value_exn (Module.file source ~ml_kind)))
                   (let* exe, flags, args = driver_and_flags in
                    let dir =
                      Path.build (Super_context.context sctx).build_dir
                    in
                    Command.run' ~dir
                      (Ok (Path.build exe))
                      [ As args
                      ; Command.Ml_kind.ppx_driver_flag ml_kind
                      ; Dep (Module.File.path src)
                      ; As flags
                      ]))))
  in
  Staged.stage @@ fun ~(source : Module.t) ~ast ->
  Module_name.Per_item.get lint (Module.name source) ~source ~ast

let pp_one_module sctx ~lib_name ~scope ~preprocessor_deps
    ~(lint_module : source:_ -> ast:_ -> unit Memo.t) ~sandbox ~dir ~expander
    (pp : _ Preprocess.Without_future_syntax.t) =
  let open Action_builder.O in
  match pp with
  | No_preprocessing ->
    Staged.stage @@ fun m ~lint ->
    let open Memo.O in
    let* ast =
      let sandbox = sandbox_of_setting sandbox in
      setup_dialect_rules sctx ~sandbox ~dir ~expander ~lib_name m
    in
    let+ () = Memo.when_ lint (fun () -> lint_module ~ast ~source:m) in
    ast
  | Action (loc, action) ->
    Staged.stage @@ fun m ~lint ->
    let open Memo.O in
    let* ast =
      let sandbox = sandbox_of_setting sandbox in
      pped_module m ~f:(fun ml_kind src dst ->
          let* action =
            action_for_pp_with_target ~dir ~sandbox ~loc ~expander ~action
              ~ml_kind ~src ~target:dst ~lib_name
          in
          Super_context.add_rule sctx ~loc ~dir
            (let open Action_builder.With_targets.O in
            Action_builder.with_no_targets preprocessor_deps >>> action))
      >>= setup_dialect_rules sctx ~sandbox ~dir ~expander ~lib_name
    in
    let+ () = Memo.when_ lint (fun () -> lint_module ~ast ~source:m) in
    ast
  | Pps { loc; pps; flags; staged } ->
    Staged.stage
    @@
    if staged then
      let dash_ppx_flag =
        Action_builder.memoize ~cutoff:(List.equal String.equal) "ppx command"
          (let* () = Action_builder.return () in
           let* exe, driver, flags =
             let context = Super_context.context sctx in
             Ppx_driver.ppx_exe_driver_and_flags ~context ~expander ~loc ~scope
               ~flags ~lib_name pps
           in
           let+ () = Action_builder.path (Path.build exe)
           and+ () = preprocessor_deps
           and+ driver_flags =
             let info = Ppx_driver.Driver.info driver in
             Expander.expand_and_eval_set expander info.as_ppx_flags
               ~standard:(Action_builder.return [ "--as-ppx" ])
           in
           let driver_flags = driver_flags in
           let command =
             List.map ~f:String.quote_for_shell
               (List.concat
                  [ [ Path.reach (Path.build exe)
                        ~from:
                          (Path.build (Super_context.context sctx).build_dir)
                    ]
                  ; driver_flags
                  ; flags
                  ])
             |> String.concat ~sep:" "
           in
           [ "-ppx"; command ])
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
        let* ast =
          setup_dialect_rules sctx ~sandbox ~dir ~expander ~lib_name m
        in
        let+ () = Memo.when_ lint (fun () -> lint_module ~ast ~source:m) in
        Module.set_pp ast pp
    else
      let corrected_suffix = ".ppx-corrected" in
      let driver_and_flags =
        Action_builder.memoize
          ~cutoff:
            (Tuple.T3.equal Path.Build.equal (List.equal String.equal)
               (List.equal String.equal))
          "ppx driver and flags"
          (let* () = Action_builder.return () in
           let* exe, driver, flags =
             let context = Super_context.context sctx in
             Ppx_driver.ppx_exe_driver_and_flags ~context ~expander ~loc
               ~lib_name ~flags ~scope pps
           in
           let+ ppx_flags =
             let info = Ppx_driver.Driver.info driver in
             Ppx_driver.driver_flags expander ~corrected_suffix
               ~driver_flags:info.flags
               ~standard:(Action_builder.return [ "--as-ppx" ])
           in
           (exe, ppx_flags, flags))
      in
      let sandbox = sandbox_of_setting sandbox in
      fun m ~lint ->
        let open Memo.O in
        let* ast =
          setup_dialect_rules sctx ~sandbox ~dir ~expander ~lib_name m
        in
        let* () = Memo.when_ lint (fun () -> lint_module ~ast ~source:m) in
        pped_module ast ~f:(fun ml_kind src dst ->
            Super_context.add_rule sctx ~loc ~dir
              (promote_correction_with_target ~suffix:corrected_suffix
                 (Path.as_in_build_dir_exn
                    (Option.value_exn (Module.file m ~ml_kind)))
                 (Action_builder.with_file_targets ~file_targets:[ dst ]
                    (let open Action_builder.O in
                    preprocessor_deps
                    >>> let* exe, flags, args = driver_and_flags in
                        let dir =
                          Path.build (Super_context.context sctx).build_dir
                        in
                        Command.run' ~dir
                          (Ok (Path.build exe))
                          [ As args
                          ; A "-o"
                          ; Path (Path.build dst)
                          ; Command.Ml_kind.ppx_driver_flag ml_kind
                          ; Dep (Path.build src)
                          ; As flags
                          ]
                        >>| Action.Full.add_sandbox sandbox))))

let make sctx ~dir ~expander ~lint ~preprocess ~preprocessor_deps
    ~instrumentation_deps ~lib_name =
  let scope = Expander.scope_host expander in
  let preprocessor_deps = preprocessor_deps @ instrumentation_deps in
  let preprocess =
    Module_name.Per_item.map preprocess ~f:(fun pp ->
        Preprocess.remove_future_syntax ~for_:Compiler pp
          (Super_context.context sctx).version)
  in
  let preprocessor_deps, sandbox =
    Dep_conf_eval.unnamed preprocessor_deps ~expander
  in
  let sandbox =
    match
      Sandbox_config.equal Sandbox_config.no_special_requirements sandbox
    with
    | false -> `Set_by_user sandbox
    | true ->
      let project = Scope.project scope in
      let dune_version = Dune_project.dune_version project in
      `Default
        (if dune_version >= (3, 3) then Sandbox_config.needs_sandboxing
        else sandbox)
  in
  let preprocessor_deps =
    Action_builder.memoize "preprocessor deps" preprocessor_deps
  in
  let lint_module =
    let sandbox = sandbox_of_setting sandbox in
    Staged.unstage
      (lint_module sctx ~sandbox ~dir ~expander ~lint ~lib_name ~scope)
  in
  Module_name.Per_item.map preprocess ~f:(fun spec ->
      Staged.unstage
      @@ pp_one_module sctx ~lib_name ~scope ~preprocessor_deps ~lint_module
           ~sandbox ~dir ~expander spec)
  |> Pp_spec.make

let ppx_exe sctx ~scope pp =
  let open Resolve.Memo.O in
  let+ libs = Lib.DB.resolve_pps (Scope.libs scope) [ (Loc.none, pp) ] in
  Ppx_driver.ppx_driver_exe sctx libs

let pped_modules_map preprocess v =
  let map =
    Module_name.Per_item.map preprocess ~f:(fun pp ->
        match Preprocess.remove_future_syntax ~for_:Compiler pp v with
        | No_preprocessing -> Module.ml_source
        | Action (_, _) -> fun m -> Module.ml_source (Module.pped m)
        | Pps { loc = _; pps = _; flags = _; staged } ->
          if staged then Module.ml_source
          else fun m -> Module.pped (Module.ml_source m))
  in
  Staged.stage (fun m -> Module_name.Per_item.get map (Module.name m) m)
