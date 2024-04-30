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
            get ~loc lib
            >>= function
            | None ->
              Resolve.Memo.fail
                (User_error.make
                   ~loc
                   [ Pp.textf
                       "%S is not an %s"
                       (Lib_name.to_string name)
                       (desc ~plural:false)
                   ])
            | Some t -> Resolve.Memo.return t))
        >>| Resolve.List.map ~f:Fun.id
      in
      { info
      ; lib
      ; runner_libraries = Resolve.Memo.List.map info.runner_libraries ~f:resolve
      ; extends
      }
    ;;

    let public_info t =
      let open Resolve.Memo.O in
      let+ runner_libraries = t.runner_libraries
      and+ extends = Memo.return t.extends in
      { Info.loc = t.info.loc
      ; flags = t.info.flags
      ; list_partitions_flags = t.info.list_partitions_flags
      ; generate_runner = t.info.generate_runner
      ; runner_libraries =
          List.map2 t.info.runner_libraries runner_libraries ~f:(fun (loc, _) lib ->
            loc, Lib.name lib)
      ; extends =
          List.map2 t.info.extends extends ~f:(fun (loc, _) t -> loc, Lib.name t.lib)
      }
    ;;
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
          }
        =
        c
      in
      let loc = lib.buildable.loc in
      let lib_name = snd lib.name in
      let inline_test_name =
        sprintf "%s.inline-tests" (Lib_name.Local.to_string lib_name)
      in
      let inline_test_dir = Path.Build.relative dir ("." ^ inline_test_name) in
      let obj_dir = Obj_dir.make_exe ~dir:inline_test_dir ~name:inline_test_name in
      let name =
        sprintf "inline_test_runner_%s" (Lib_name.Local.to_string (snd lib.name))
      in
      let main_module =
        let name = Module_name.of_string name in
        Module.generated ~kind:Impl ~src_dir:inline_test_dir [ name ]
      in
      let open Memo.O in
      let modules = Modules.With_vlib.singleton_exe main_module in
      let runner_libs =
        let open Resolve.Memo.O in
        let* libs =
          Resolve.Memo.List.concat_map backends ~f:(fun (backend : Backend.t) ->
            backend.runner_libraries)
        in
        let* lib = Lib.DB.resolve (Scope.libs scope) (loc, Library.best_name lib) in
        let* more_libs =
          Resolve.Memo.List.map info.libraries ~f:(Lib.DB.resolve (Scope.libs scope))
        in
        Lib.closure ~linking:true ((lib :: libs) @ more_libs)
      in
      (* Generate the runner file *)
      let* () =
        Super_context.add_rule
          sctx
          ~dir
          ~loc
          (let target =
             Module.file main_module ~ml_kind:Impl
             |> Option.value_exn
             |> Path.as_in_build_dir_exn
           in
           let files ml_kind =
             Value.L.paths (List.filter_map source_modules ~f:(Module.file ~ml_kind))
           in
           let bindings =
             Pform.Map.of_list_exn
               [ Var Impl_files, files Impl; Var Intf_files, files Intf ]
           in
           let expander = Expander.add_bindings expander ~bindings in
           let action =
             let open Action_builder.With_targets.O in
             let+ actions =
               List.filter_map backends ~f:(fun (backend : Backend.t) ->
                 Option.map backend.info.generate_runner ~f:(fun (loc, action) ->
                   Action_unexpanded.expand_no_targets
                     action
                     ~loc
                     ~expander
                     ~chdir:dir
                     ~deps:[]
                     ~what:"inline test generators"))
               |> Action_builder.all
               |> Action_builder.with_no_targets
             in
             Action.Full.reduce actions
             |> Action.Full.map ~f:(Action.with_stdout_to target)
           in
           Action_builder.With_targets.add ~file_targets:[ target ] action)
      and* cctx =
        let package = Library.package lib in
        let* ocaml_flags =
          Buildable_rules.ocaml_flags sctx ~dir info.executable_ocaml_flags
        in
        let flags = Ocaml_flags.append_common ocaml_flags [ "-w"; "-24"; "-g" ] in
        let js_of_ocaml =
          Js_of_ocaml.In_context.make
            ~dir
            { lib.buildable.js_of_ocaml with javascript_files = [] }
        in
        Compilation_context.create
          ()
          ~super_context:sctx
          ~scope
          ~obj_dir
          ~modules
          ~opaque:(Explicit false)
          ~requires_compile:runner_libs
          ~requires_link:(Memo.lazy_ (fun () -> runner_libs))
          ~flags
          ~js_of_ocaml:(Some js_of_ocaml)
          ~melange_package_name:None
          ~package
      in
      let linkages =
        let modes =
          if Mode_conf.Set.mem info.modes Javascript
          then Mode_conf.Set.add info.modes Byte
          else info.modes
        in
        let ocaml = Compilation_context.ocaml cctx in
        List.concat_map (Mode_conf.Set.to_list modes) ~f:(fun (mode : Mode_conf.t) ->
          match mode with
          | Native -> [ Exe.Linkage.native ]
          | Best -> [ Exe.Linkage.native_or_custom ocaml ]
          | Byte -> [ Exe.Linkage.custom_with_ext ~ext:".bc" ocaml.version ]
          | Javascript -> [ Exe.Linkage.js; Exe.Linkage.byte_for_jsoo ])
      in
      let* (_ : Exe.dep_graphs) =
        let link_args =
          let open Action_builder.O in
          let+ link_args_info =
            Expander.expand_and_eval_set
              expander
              info.executable_link_flags
              ~standard:(Action_builder.return [ "-linkall" ])
          in
          Command.Args.As link_args_info
        in
        Exe.build_and_link
          cctx
          ~program:{ name; main_module_name = Module.name main_module; loc }
          ~linkages
          ~link_args
          ~promote:None
      in
      let partitions_flags : string list Action_builder.t option =
        match
          List.filter_map backends ~f:(fun backend -> backend.info.list_partitions_flags)
        with
        | [] -> None
        | flags ->
          let flags =
            let open Action_builder.O in
            let+ l =
              let expander =
                let bindings =
                  Pform.Map.singleton
                    (Pform.Var Library_name)
                    [ Value.String (Lib_name.Local.to_string (snd lib.name)) ]
                in
                Expander.add_bindings expander ~bindings
              in
              List.map
                flags
                ~f:
                  (Expander.expand_and_eval_set
                     expander
                     ~standard:(Action_builder.return []))
              |> Action_builder.all
            in
            List.concat l
          in
          Some flags
      in
      let sandbox =
        let project = Scope.project scope in
        if Dune_project.dune_version project < (3, 5)
        then Sandbox_config.no_special_requirements
        else Sandbox_config.needs_sandboxing
      in
      let deps, sandbox = Dep_conf_eval.unnamed ~sandbox info.deps ~expander in
      let action (mode : Mode_conf.t) (flags : string list Action_builder.t)
        : Action.t Action_builder.t
        =
        (* [action] needs to run from [dir] as we use [dir] to resolve
           the exe path in case of a custom [runner] *)
        let ext =
          match mode with
          | Native | Best -> ".exe"
          | Javascript -> Js_of_ocaml.Ext.exe
          | Byte -> ".bc"
        in
        let custom_runner =
          match mode with
          | Native | Best | Byte -> None
          | Javascript -> Some Jsoo_rules.runner
        in
        let exe = Path.build (Path.Build.relative inline_test_dir (name ^ ext)) in
        let open Action_builder.O in
        let+ action =
          match custom_runner with
          | None ->
            let+ flags = flags in
            Action.run (Ok exe) flags
          | Some runner ->
            let* prog =
              Super_context.resolve_program
                ~dir
                sctx
                ~where:Original_path
                ~loc:(Some loc)
                runner
            and* flags = flags in
            let action =
              Action.run prog (Path.reach exe ~from:(Path.build dir) :: flags)
            in
            (* jeremiedimino: it feels like this pattern should be pushed
               into [resolve_program] directly *)
            (match prog with
             | Error _ -> Action_builder.return action
             | Ok p -> Action_builder.path p >>> Action_builder.return action)
        and+ () = deps
        and+ () = Action_builder.path exe in
        Action.chdir (Path.build dir) action
      in
      let flags partition : string list Action_builder.t =
        let flags =
          List.map backends ~f:(fun (backend : Backend.t) -> backend.info.flags)
          @ [ info.flags ]
        in
        let bindings =
          Pform.Map.singleton
            (Pform.Var Library_name)
            [ Value.String (Lib_name.Local.to_string (snd lib.name)) ]
        in
        let bindings =
          match partition with
          | None -> bindings
          | Some p -> Pform.Map.add_exn bindings (Pform.Var Partition) [ Value.String p ]
        in
        let expander = Expander.add_bindings expander ~bindings in
        let open Action_builder.O in
        let+ l =
          List.map
            flags
            ~f:
              (Expander.expand_and_eval_set expander ~standard:(Action_builder.return []))
          |> Action_builder.all
        in
        List.concat l
      in
      let source_files = List.concat_map source_modules ~f:Module.sources in
      Memo.parallel_iter_seq
        (Mode_conf.Set.to_seq info.modes)
        ~f:(fun (mode : Mode_conf.t) ->
          let partition_file =
            Path.Build.relative inline_test_dir ("partitions-" ^ Mode_conf.to_string mode)
          in
          let* () =
            match partitions_flags with
            | None -> Memo.return ()
            | Some partitions_flags ->
              let open Action_builder.O in
              action mode partitions_flags
              >>| Action.Full.make ~sandbox
              |> Action_builder.with_stdout_to partition_file
              |> Super_context.add_rule sctx ~dir ~loc
          in
          let* runtest_alias =
            match mode with
            | Native | Best | Byte -> Memo.return Alias0.runtest
            | Javascript -> Jsoo_rules.js_of_ocaml_runtest_alias ~dir
          in
          Super_context.add_alias_action
            sctx
            ~dir
            ~loc:info.loc
            (Alias.make ~dir runtest_alias)
            (let open Action_builder.O in
             let+ actions =
               let* partitions_flags =
                 match partitions_flags with
                 | None -> Action_builder.return [ None ]
                 | Some _ ->
                   let+ partitions =
                     Action_builder.lines_of (Path.build partition_file)
                   in
                   List.map ~f:(fun x -> Some x) partitions
               in
               List.map partitions_flags ~f:(fun p -> action mode (flags p))
               |> Action_builder.all
             and+ () = Action_builder.paths source_files in
             match actions with
             | [] -> Action.Full.empty
             | _ :: _ ->
               let run_tests = Action.concurrent actions in
               let diffs =
                 List.map source_files ~f:(fun fn ->
                   Path.as_in_build_dir_exn fn
                   |> Path.Build.extend_basename ~suffix:".corrected"
                   |> Action.diff ~optional:true fn)
                 |> Action.concurrent
               in
               Action.Full.make ~sandbox @@ Action.progn [ run_tests; diffs ]))
    ;;

    let gen_rules c ~(info : Info.t) ~backends =
      let open Memo.O in
      let { dir; Sub_system.Library_compilation_context.super_context = sctx; _ } = c in
      let* expander = Super_context.expander sctx ~dir in
      let* enabled_if = Expander.eval_blang expander info.enabled_if in
      if enabled_if
      then gen_rules c ~expander ~info ~backends
      else (
        let alias = Alias.make Alias0.runtest ~dir in
        Simple_rules.Alias_rules.add_empty sctx ~alias ~loc:info.loc)
    ;;
  end)

let linkme = ()
