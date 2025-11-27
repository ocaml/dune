open Import
open Memo.O
module Mode_conf = Inline_tests_info.Mode_conf

let action
      sctx
      ~deps
      ~loc
      ~dir
      ~inline_test_dir
      ~runner_name
      (mode : Mode_conf.t)
      (flags : string list Action_builder.t)
  : Action.t Action_builder.t
  =
  let exe =
    let ext =
      match mode with
      | Native | Best -> ".exe"
      | Jsoo mode -> Js_of_ocaml.Ext.exe ~mode
      | Byte -> ".bc"
    in
    Path.build (Path.Build.relative inline_test_dir (runner_name ^ ext))
  in
  let open Action_builder.O in
  let+ action =
    (* [action] needs to run from [dir] as we use [dir] to resolve
       the exe path in case of a custom [runner] *)
    match
      match mode with
      | Native | Best | Byte -> None
      | Jsoo _ -> Some Jsoo_rules.runner
    with
    | None -> flags >>| Action.run (Ok exe)
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
        Path.reach exe ~from:(Path.build dir) :: flags
        |> Action.run prog
        |> Action_builder.return
      in
      (* jeremiedimino: it feels like this pattern should be pushed
         into [resolve_program] directly *)
      (match prog with
       | Error _ -> action
       | Ok p -> Action_builder.path p >>> action)
  and+ () = deps
  and+ () = Action_builder.path exe
  and+ () =
    match mode with
    | Native | Best | Byte | Jsoo JS -> Action_builder.return ()
    | Jsoo Wasm ->
      Path.Build.relative inline_test_dir (runner_name ^ Js_of_ocaml.Ext.wasm_dir)
      |> Path.build
      |> Action_builder.path
  in
  Action.chdir (Path.build dir) action
;;

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

module Info = Inline_tests_info.Tests

let flags ~(info : Info.t) ~expander ~backends ~lib_name ~partition =
  let expander =
    let bindings =
      let bindings =
        Pform.Map.singleton
          (Pform.Var Library_name)
          [ Value.String (Lib_name.Local.to_string lib_name) ]
      in
      match partition with
      | None -> bindings
      | Some p -> Pform.Map.add_exn bindings (Pform.Var Partition) [ Value.String p ]
    in
    Expander.add_bindings expander ~bindings
  in
  let open Action_builder.O in
  List.map backends ~f:(fun (backend : Backend.t) -> backend.info.flags) @ [ info.flags ]
  |> List.map
       ~f:(Expander.expand_and_eval_set expander ~standard:(Action_builder.return []))
  |> Action_builder.all
  >>| List.concat
;;

let partition_flags ~expander ~lib_name ~(backends : Backend.t list) =
  match
    List.filter_map backends ~f:(fun backend -> backend.info.list_partitions_flags)
  with
  | [] -> None
  | flags ->
    let flags =
      let open Action_builder.O in
      let expander =
        let bindings =
          Pform.Map.singleton
            (Pform.Var Library_name)
            [ Value.String (Lib_name.Local.to_string lib_name) ]
        in
        Expander.add_bindings expander ~bindings
      in
      List.map
        flags
        ~f:(Expander.expand_and_eval_set expander ~standard:(Action_builder.return []))
      |> Action_builder.all
      >>| List.concat
    in
    Some flags
;;

include Sub_system.Register_end_point (struct
    module Backend = Backend
    module Info = Inline_tests_info.Tests

    let gen_rules
          { Sub_system.Library_compilation_context.super_context = sctx
          ; dir
          ; stanza = lib
          ; scope
          ; source_modules
          ; compile_info = _
          }
          ~expander
          ~(info : Info.t)
          ~backends
      =
      let loc = lib.buildable.loc in
      let inline_test_dir =
        let lib_name = snd lib.name in
        Path.Build.relative dir (Inline_tests_info.inline_test_dirname lib_name)
      in
      let runner_name = Inline_tests_info.inline_test_runner in
      let main_module =
        let name = Module_name.of_string "main" in
        Module.generated ~kind:Impl ~src_dir:inline_test_dir [ name ]
      in
      (* Generate the runner file *)
      let js_of_ocaml =
        Js_of_ocaml.In_context.make ~dir lib.buildable.js_of_ocaml
        |> Js_of_ocaml.Mode.Pair.map ~f:(fun (x : Js_of_ocaml.In_context.t) ->
          { x with javascript_files = []; wasm_files = [] })
      in
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
           let action =
             let open Action_builder.With_targets.O in
             let+ actions =
               let expander =
                 let bindings =
                   let files ml_kind =
                     List.filter_map source_modules ~f:(Module.file ~ml_kind)
                     |> Value.L.paths
                   in
                   Pform.Map.of_list_exn
                     [ Var Impl_files, files Impl; Var Intf_files, files Intf ]
                 in
                 Expander.add_bindings expander ~bindings
               in
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
        let* flags =
          let+ ocaml_flags =
            Buildable_rules.ocaml_flags sctx ~dir info.executable_ocaml_flags
          in
          Ocaml_flags.append_common ocaml_flags [ "-w"; "-24"; "-g" ]
        in
        let obj_dir = Obj_dir.make_exe ~dir:inline_test_dir ~name:"t" in
        let modules = Modules.With_vlib.singleton_exe main_module in
        let runner_libs =
          let lib_db = Scope.libs scope in
          let open Resolve.Memo.O in
          let* libs =
            Resolve.Memo.List.concat_map backends ~f:(fun (backend : Backend.t) ->
              backend.runner_libraries)
          in
          let* arguments =
            Resolve.Memo.lift_memo
            @@ Memo.List.map info.arguments ~f:(fun (loc, dep) ->
              let open Memo.O in
              let+ dep = Lib.DB.resolve lib_db (loc, dep) in
              loc, dep)
          in
          let* lib =
            let* lib = Lib.DB.resolve lib_db (loc, Library.best_name lib) in
            Resolve.Memo.lift
            @@ Lib.Parameterised.instantiate
                 ~from:`inline_tests
                 ~loc
                 lib
                 arguments
                 ~parent_parameters:[]
          in
          let* more_libs =
            Resolve.Memo.List.map info.libraries ~f:(Lib.DB.resolve lib_db)
          in
          Lib.closure ~linking:true ((lib :: libs) @ more_libs)
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
          ~js_of_ocaml:(Js_of_ocaml.Mode.Pair.map ~f:Option.some js_of_ocaml)
          ~melange_package_name:None
          ~package
      in
      let* modes =
        let+ jsoo_enabled_modes =
          Jsoo_rules.jsoo_enabled_modes ~expander ~dir ~in_context:js_of_ocaml
        in
        Mode_conf.Set.to_list info.modes
        |> List.filter ~f:(fun (mode : Mode_conf.t) ->
          match mode with
          | Native | Best | Byte -> true
          | Jsoo mode -> Js_of_ocaml.Mode.Pair.select ~mode jsoo_enabled_modes)
      in
      let* (_ : Exe.dep_graphs) =
        let* linkages =
          let+ jsoo =
            let+ jsoo_is_whole_program =
              Jsoo_rules.jsoo_is_whole_program sctx ~dir ~in_context:js_of_ocaml
            in
            if
              List.exists modes ~f:(fun mode ->
                match (mode : Mode_conf.t) with
                | Jsoo mode -> Js_of_ocaml.Mode.Pair.select ~mode jsoo_is_whole_program
                | Native | Best | Byte -> false)
            then [ Exe.Linkage.byte_for_jsoo ]
            else []
          in
          jsoo
          @
          let ocaml = Compilation_context.ocaml cctx in
          List.map modes ~f:(fun (mode : Mode_conf.t) ->
            match mode with
            | Native -> Exe.Linkage.native
            | Best -> Exe.Linkage.native_or_custom ocaml
            | Byte -> Exe.Linkage.custom_with_ext ~ext:".bc" ocaml.version
            | Jsoo JS -> Exe.Linkage.js
            | Jsoo Wasm -> Exe.Linkage.wasm)
        in
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
          ~program:{ name = runner_name; main_module_name = Module.name main_module; loc }
          ~linkages
          ~link_args
          ~promote:None
      in
      let lib_name = snd lib.name in
      let partitions_flags = partition_flags ~expander ~lib_name ~backends in
      let deps, sandbox =
        let sandbox =
          let project = Scope.project scope in
          if Dune_project.dune_version project < (3, 5)
          then Sandbox_config.no_special_requirements
          else Sandbox_config.needs_sandboxing
        in
        Dep_conf_eval.unnamed ~sandbox info.deps ~expander
      in
      let action = action sctx ~deps ~loc ~dir ~inline_test_dir ~runner_name in
      Memo.parallel_iter modes ~f:(fun (mode : Mode_conf.t) ->
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
          | Jsoo mode -> Jsoo_rules.js_of_ocaml_runtest_alias ~dir ~mode
        in
        let alias =
          [ Alias.Name.to_string runtest_alias; Lib_name.Local.to_string lib_name ]
          |> String.concat ~sep:"-"
          |> Alias.Name.of_string
          |> Alias.make ~dir
        in
        let* () =
          let runtest_alias = Alias.make ~dir runtest_alias in
          Dep.alias alias
          |> Action_builder.dep
          |> Rules.Produce.Alias.add_deps runtest_alias ~loc
        in
        Super_context.add_alias_action
          sctx
          ~dir
          ~loc:info.loc
          alias
          (let open Action_builder.O in
           let source_files = List.concat_map source_modules ~f:Module.sources in
           let+ actions =
             (match partitions_flags with
              | None -> Action_builder.return [ None ]
              | Some _ ->
                Path.build partition_file
                |> Action_builder.lines_of
                >>| List.map ~f:(fun x -> Some x))
             >>| List.map ~f:(fun partition ->
               flags ~info ~expander ~backends ~lib_name:(snd lib.name) ~partition
               |> action mode)
             >>= Action_builder.all
           and+ () = Action_builder.paths source_files in
           match actions with
           | [] -> Action.Full.empty
           | _ :: _ ->
             let run_tests = Action.concurrent actions in
             let diffs =
               List.map source_files ~f:(fun fn ->
                 Path.as_in_build_dir_exn fn
                 |> Path.Build.extend_basename ~suffix:".corrected"
                 |> Promote.Diff_action.diff ~optional:true fn)
               |> Action.concurrent
             in
             Action.Full.make ~sandbox @@ Action.progn [ run_tests; diffs ]))
    ;;

    let gen_rules
          ({ dir; Sub_system.Library_compilation_context.super_context = sctx; _ } as c)
          ~(info : Info.t)
          ~backends
      =
      let* expander = Super_context.expander sctx ~dir in
      Expander.eval_blang expander info.enabled_if
      >>= function
      | true -> gen_rules c ~expander ~info ~backends
      | false ->
        let alias = Alias.make Alias0.runtest ~dir in
        Simple_rules.Alias_rules.add_empty sctx ~alias ~loc:info.loc
    ;;
  end)

let linkme = ()
