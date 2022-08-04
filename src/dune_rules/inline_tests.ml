open Import

module Backend = struct
  module M = struct
    module Info = Inline_tests_info.Backend

    type t =
      { info : Info.t
      ; lib : Lib.t
      ; runner_libraries : Lib.t list Resolve.Memo.t
      ; extends : t list Resolve.t
      }

    let desc ~plural = "inline tests backend" ^ if plural then "s" else ""

    let desc_article = "an"

    let lib t = t.lib

    let extends t = t.extends

    let instantiate ~resolve ~get lib (info : Info.t) =
      let open Memo.O in
      let+ extends =
        Memo.parallel_map info.extends ~f:(fun ((loc, name) as x) ->
            Resolve.Memo.bind (resolve x) ~f:(fun lib ->
                get ~loc lib >>= function
                | None ->
                  Resolve.Memo.fail
                    (User_error.make ~loc
                       [ Pp.textf "%S is not an %s" (Lib_name.to_string name)
                           (desc ~plural:false)
                       ])
                | Some t -> Resolve.Memo.return t))
        >>| Resolve.List.map ~f:Fun.id
      in
      { info
      ; lib
      ; runner_libraries =
          Resolve.Memo.List.map info.runner_libraries ~f:resolve
      ; extends
      }

    let public_info t =
      let open Resolve.Memo.O in
      let+ runner_libraries = t.runner_libraries
      and+ extends = Memo.return t.extends in
      { Info.loc = t.info.loc
      ; flags = t.info.flags
      ; generate_runner = t.info.generate_runner
      ; runner_libraries =
          List.map2 t.info.runner_libraries runner_libraries
            ~f:(fun (loc, _) lib -> (loc, Lib.name lib))
      ; extends =
          List.map2 t.info.extends extends ~f:(fun (loc, _) t ->
              (loc, Lib.name t.lib))
      }
  end

  include M
  include Sub_system.Register_backend (M)
end

include Sub_system.Register_end_point (struct
  module Backend = Backend
  module Mode_conf = Inline_tests_info.Mode_conf
  module Info = Inline_tests_info.Tests

  let gen_rules c ~expander ~(info : Info.t) ~backends =
    let { Sub_system.Library_compilation_context.super_context = sctx
        ; dir
        ; stanza = lib
        ; scope
        ; source_modules
        ; compile_info = _
        } =
      c
    in
    let loc = lib.buildable.loc in
    let lib_name = snd lib.name in
    let inline_test_name =
      sprintf "%s.inline-tests" (Lib_name.Local.to_string lib_name)
    in
    let inline_test_dir = Path.Build.relative dir ("." ^ inline_test_name) in
    let obj_dir =
      Obj_dir.make_exe ~dir:inline_test_dir ~name:inline_test_name
    in
    let name =
      sprintf "inline_test_runner_%s" (Lib_name.Local.to_string (snd lib.name))
    in
    let main_module =
      let name = Module_name.of_string name in
      let src_dir = Path.build inline_test_dir in
      Module.generated ~src_dir name
    in
    let open Memo.O in
    let modules = Modules.singleton_exe main_module in
    let runner_libs =
      let open Resolve.Memo.O in
      let* libs =
        Resolve.Memo.List.concat_map backends ~f:(fun (backend : Backend.t) ->
            backend.runner_libraries)
      in
      let* lib =
        Lib.DB.resolve (Scope.libs scope) (loc, Dune_file.Library.best_name lib)
      in
      let* more_libs =
        Resolve.Memo.List.map info.libraries
          ~f:(Lib.DB.resolve (Scope.libs scope))
      in
      Lib.closure ~linking:true ((lib :: libs) @ more_libs)
    in
    (* Generate the runner file *)
    let* () =
      Super_context.add_rule sctx ~dir ~loc
        (let target =
           Module.file main_module ~ml_kind:Impl
           |> Option.value_exn |> Path.as_in_build_dir_exn
         in
         let files ml_kind =
           Value.L.paths
             (List.filter_map source_modules ~f:(Module.file ~ml_kind))
         in
         let bindings =
           Pform.Map.of_list_exn
             [ (Var Impl_files, files Impl); (Var Intf_files, files Intf) ]
         in
         let expander = Expander.add_bindings expander ~bindings in
         let action =
           let open Action_builder.With_targets.O in
           let+ actions =
             Action_builder.with_no_targets
               (Action_builder.all
                  (List.filter_map backends ~f:(fun (backend : Backend.t) ->
                       Option.map backend.info.generate_runner
                         ~f:(fun (loc, action) ->
                           Action_unexpanded.expand_no_targets action ~loc
                             ~expander ~deps:[] ~what:"inline test generators"))))
           in
           Action.Full.reduce actions
           |> Action.Full.map ~f:(Action.with_stdout_to target)
         in
         Action_builder.With_targets.add ~file_targets:[ target ] action)
    and* cctx =
      let package = Dune_file.Library.package lib in
      let* ocaml_flags =
        Super_context.ocaml_flags sctx ~dir info.executable_ocaml_flags
      in
      let flags = Ocaml_flags.append_common ocaml_flags [ "-w"; "-24"; "-g" ] in
      let js_of_ocaml =
        Js_of_ocaml.In_context.make ~dir
          { lib.buildable.js_of_ocaml with javascript_files = [] }
      in
      Compilation_context.create () ~super_context:sctx ~expander ~scope
        ~obj_dir ~modules ~opaque:(Explicit false) ~requires_compile:runner_libs
        ~requires_link:(Memo.lazy_ (fun () -> runner_libs))
        ~flags ~js_of_ocaml:(Some js_of_ocaml) ~package
    in
    let linkages =
      let modes =
        if Mode_conf.Set.mem info.modes Javascript then
          Mode_conf.Set.add info.modes Byte
        else info.modes
      in
      List.map (Mode_conf.Set.to_list modes) ~f:(fun (mode : Mode_conf.t) ->
          match mode with
          | Native -> Exe.Linkage.native
          | Best -> Exe.Linkage.native_or_custom (Super_context.context sctx)
          | Byte -> Exe.Linkage.byte
          | Javascript -> Exe.Linkage.js)
    in
    let* (_ : Exe.dep_graphs) =
      let link_args =
        let open Action_builder.O in
        let+ link_args_info =
          Expander.expand_and_eval_set expander info.executable_link_flags
            ~standard:(Action_builder.return [ "-linkall" ])
        in
        Command.Args.As link_args_info
      in
      Exe.build_and_link cctx
        ~program:{ name; main_module_name = Module.name main_module; loc }
        ~linkages ~link_args ~promote:None
    in
    let flags =
      let flags =
        List.map backends ~f:(fun backend -> backend.Backend.info.flags)
        @ [ info.flags ]
      in
      let bindings =
        Pform.Map.singleton (Var Library_name)
          [ Value.String (Lib_name.Local.to_string (snd lib.name)) ]
      in
      let expander = Expander.add_bindings expander ~bindings in
      let open Action_builder.O in
      let+ l =
        List.map flags
          ~f:
            (Expander.expand_and_eval_set expander
               ~standard:(Action_builder.return []))
        |> Action_builder.all
      in
      List.concat l
    in
    let source_files = List.concat_map source_modules ~f:Module.sources in
    Memo.parallel_iter_set
      (module Mode_conf.Set)
      info.modes
      ~f:(fun (mode : Mode_conf.t) ->
        let ext =
          match mode with
          | Native | Best -> ".exe"
          | Javascript -> ".bc.js"
          | Byte -> ".bc"
        in
        let custom_runner =
          match mode with
          | Native | Best | Byte -> None
          | Javascript -> Some Jsoo_rules.runner
        in
        let* runtest_alias =
          match mode with
          | Native | Best | Byte -> Memo.return Alias.Name.runtest
          | Javascript -> Super_context.js_of_ocaml_runtest_alias sctx ~dir
        in
        Super_context.add_alias_action sctx ~dir ~loc:(Some info.loc)
          (Alias.make ~dir runtest_alias)
          (let exe =
             Path.build (Path.Build.relative inline_test_dir (name ^ ext))
           in
           let open Action_builder.O in
           let deps, sandbox = Dep_conf_eval.unnamed info.deps ~expander in
           let+ () = deps
           and+ () = Action_builder.paths source_files
           and+ () = Action_builder.path exe
           and+ action =
             match custom_runner with
             | None ->
               let+ flags = flags in
               Action.run (Ok exe) flags
             | Some runner -> (
               let* prog =
                 Action_builder.of_memo
                   (Super_context.resolve_program ~dir sctx ~loc:(Some loc)
                      runner)
               and* flags = flags in
               let action =
                 Action.run prog (Path.reach exe ~from:(Path.build dir) :: flags)
               in
               (* jeremiedimino: it feels like this pattern should be pushed
                  into [resolve_program] directly *)
               match prog with
               | Error _ -> Action_builder.return action
               | Ok p -> Action_builder.path p >>> Action_builder.return action)
           in
           let run_tests = Action.chdir (Path.build dir) action in
           Action.Full.make ~sandbox
           @@ Action.progn
                (run_tests
                :: List.map source_files ~f:(fun fn ->
                       Action.diff ~optional:true fn
                         (Path.Build.extend_basename
                            (Path.as_in_build_dir_exn fn)
                            ~suffix:".corrected")))))

  let gen_rules c ~(info : Info.t) ~backends =
    let open Memo.O in
    let { dir; Sub_system.Library_compilation_context.super_context = sctx; _ }
        =
      c
    in
    let* expander = Super_context.expander sctx ~dir in
    let* enabled_if = Expander.eval_blang expander info.enabled_if in
    if enabled_if then gen_rules c ~expander ~info ~backends
    else
      let alias = Alias.runtest ~dir in
      Simple_rules.Alias_rules.add_empty sctx ~alias ~loc:(Some info.loc)
end)

let linkme = ()
