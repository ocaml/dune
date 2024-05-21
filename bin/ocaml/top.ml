open Import

let doc =
  "Print a list of toplevel directives for including directories and loading cma files."
;;

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|Print a list of toplevel directives for including directories and loading cma files.|}
  ; `P
      {|The output of $(b,dune top) should be evaluated in a toplevel
          to make a library available there.|}
  ; `Blocks Common.help_secs
  ]
;;

let info = Cmd.info "top" ~doc ~man

let link_deps sctx link =
  let open Memo.O in
  Memo.parallel_map link ~f:(fun t ->
    Dune_rules.Lib_flags.link_deps sctx t Dune_rules.Link_mode.Byte)
  >>| List.concat
;;

let files_to_load_of_requires sctx requires =
  let open Memo.O in
  let* files = link_deps sctx requires in
  let+ () = Memo.parallel_iter files ~f:Build_system.build_file in
  List.filter files ~f:(fun p ->
    let ext = Path.extension p in
    ext = Ocaml.Mode.compiled_lib_ext Byte || ext = Ocaml.Cm_kind.ext Cmo)
;;

let term =
  let+ builder = Common.Builder.term
  and+ dir = Arg.(value & pos 0 string "" & Arg.info [] ~docv:"DIR")
  and+ ctx_name = Common.context_arg ~doc:{|Select context where to build/run utop.|} in
  let common, config = Common.init builder in
  Scheduler.go ~common ~config (fun () ->
    let open Fiber.O in
    let* setup = Import.Main.setup () in
    Build_system.run_exn (fun () ->
      let open Memo.O in
      let* setup = setup in
      let sctx =
        Dune_engine.Context_name.Map.find setup.scontexts ctx_name |> Option.value_exn
      in
      let* libs =
        let dir =
          let build_dir = Super_context.context sctx |> Context.build_dir in
          Path.Build.relative build_dir (Common.prefix_target common dir)
        in
        let* db =
          let+ scope = Dune_rules.Scope.DB.find_by_dir dir in
          Dune_rules.Scope.libs scope
        in
        (* TODO why don't we read ppx as well?*)
        Dune_rules.Utop.libs_under_dir sctx ~db ~dir:(Path.build dir)
      in
      let* requires =
        Dune_rules.Resolve.Memo.read_memo (Dune_rules.Lib.closure ~linking:true libs)
      in
      let include_paths = Dune_rules.Lib_flags.L.toplevel_include_paths requires in
      let+ files_to_load = files_to_load_of_requires sctx requires in
      Dune_rules.Toplevel.print_toplevel_init_file
        { include_paths; files_to_load; uses = []; pp = None; ppx = None; code = [] }))
;;

let command = Cmd.v info term

module Module = struct
  let doc = "Print a list of toplevel directives for loading a module into the topevel."

  let man =
    [ `S "DESCRIPTION"
    ; `P doc
    ; `P
        "The module's source is evaluated in the toplevel without being sealed by the \
         mli."
    ; `P
        {|The output of $(b,dune top) should be evaluated in a toplevel
          to make the module available there.|}
    ; `Blocks Common.help_secs
    ]
  ;;

  let info = Cmd.info "top-module" ~doc ~man

  let module_directives sctx mod_ =
    let ctx = Super_context.context sctx in
    let src = Path.Build.append_source (Context.build_dir ctx) mod_ in
    let dir = Path.Build.parent_exn src in
    let filename = Path.Build.basename src in
    if Filename.extension filename = ""
    then User_error.raise [ Pp.text "file is missing an extension" ];
    let open Memo.O in
    let module_name =
      let name = Filename.remove_extension filename in
      Dune_rules.Module_name.of_string_user_error (Loc.none, name) |> User_error.ok_exn
    in
    let* expander = Super_context.expander sctx ~dir in
    let* top_module_info = Dune_rules.Top_module.find_module sctx mod_ in
    match top_module_info with
    | None -> User_error.raise [ Pp.text "no module found" ]
    | Some (_, _, _, Melange _) ->
      User_error.raise [ Pp.text "Modules belonging to `melange.emit' are not supported" ]
    | Some (module_, cctx, merlin, _) ->
      let module Compilation_context = Dune_rules.Compilation_context in
      let module Obj_dir = Dune_rules.Obj_dir in
      let module Top_module = Dune_rules.Top_module in
      let* requires =
        let* requires = Compilation_context.requires_link cctx in
        Dune_rules.Resolve.read_memo requires
      in
      let private_obj_dir = Top_module.private_obj_dir ctx mod_ in
      let include_paths =
        let libs = Dune_rules.Lib_flags.L.toplevel_include_paths requires in
        Path.Set.add libs (Path.build (Obj_dir.byte_dir private_obj_dir))
      in
      let files_to_load () =
        let+ libs, modules =
          Memo.fork_and_join
            (fun () -> files_to_load_of_requires sctx requires)
            (fun () ->
              let cmis () =
                let glob =
                  Dune_engine.File_selector.of_glob
                    ~dir:(Path.build (Obj_dir.byte_dir private_obj_dir))
                    (Dune_lang.Glob.of_string_exn Loc.none "*.cmi")
                in
                let* files = Build_system.eval_pred glob in
                Memo.parallel_iter (Filename_set.to_list files) ~f:Build_system.build_file
              in
              let cmos () =
                let obj_dir = Compilation_context.obj_dir cctx in
                let dep_graph = (Compilation_context.dep_graphs cctx).impl in
                let* modules =
                  let graph =
                    Dune_rules.Dep_graph.top_closed_implementations dep_graph [ module_ ]
                  in
                  let+ modules, _ = Action_builder.evaluate_and_collect_facts graph in
                  modules
                in
                let cmos =
                  let module Module = Dune_rules.Module in
                  let module Module_name = Dune_rules.Module_name in
                  let module_obj_name = Module.obj_name module_ in
                  List.filter_map modules ~f:(fun m ->
                    let obj_dir =
                      if Module_name.Unique.equal module_obj_name (Module.obj_name m)
                      then private_obj_dir
                      else obj_dir
                    in
                    Obj_dir.Module.cm_file obj_dir m ~kind:(Ocaml Cmo)
                    |> Option.map ~f:Path.build)
                in
                let+ (_ : Dep.Facts.t) =
                  Build_system.build_deps (Dep.Set.of_files cmos)
                in
                cmos
              in
              Memo.fork_and_join_unit cmis cmos)
        in
        libs @ modules
      in
      let pps () =
        let module Merlin = Dune_rules.Merlin in
        let pps = Merlin.pp_config merlin ctx ~expander in
        let+ pps, _ = Action_builder.evaluate_and_collect_facts pps in
        let pp = Dune_rules.Module_name.Per_item.get pps module_name in
        match pp with
        | None -> None, None
        | Some pp_flags ->
          let args = Merlin.Processed.pp_args pp_flags in
          (match Merlin.Processed.pp_kind pp_flags with
           | Pp -> Some args, None
           | Ppx -> None, Some args)
      in
      let+ (pp, ppx), files_to_load = Memo.fork_and_join pps files_to_load in
      let code =
        let modules = Dune_rules.Compilation_context.modules cctx in
        let opens_ = Dune_rules.Modules.With_vlib.local_open modules module_ in
        List.map opens_ ~f:(fun name ->
          sprintf "open %s" (Dune_rules.Module_name.to_string name))
      in
      { Dune_rules.Toplevel.files_to_load; pp; ppx; include_paths; uses = []; code }
  ;;

  let term =
    let+ builder = Common.Builder.term
    and+ module_path =
      Arg.(
        required
        & pos 0 (some string) None
        & Arg.info [] ~docv:"MODULE" ~doc:"Path to an OCaml module.")
    and+ ctx_name = Common.context_arg ~doc:{|Select context where to build/run utop.|} in
    let common, config = Common.init builder in
    Scheduler.go ~common ~config (fun () ->
      let open Fiber.O in
      let* setup = Import.Main.setup () in
      Build_system.run_exn (fun () ->
        let open Memo.O in
        let* setup = setup in
        let sctx =
          Dune_engine.Context_name.Map.find setup.scontexts ctx_name |> Option.value_exn
        in
        let+ directives =
          let module_path =
            if Filename.is_relative module_path
            then Path.Local.of_string module_path
            else (
              let root =
                (Common.root common).dir
                |> Path.of_string
                |> Path.to_absolute_filename
                |> Path.of_string
              in
              match Path.drop_prefix ~prefix:root (Path.of_string module_path) with
              | Some module_path -> module_path
              | None ->
                User_error.raise
                  [ Pp.text "Module path not a descendent of workspace root." ])
          in
          module_directives sctx (Path.Source.of_local module_path)
        in
        Dune_rules.Toplevel.print_toplevel_init_file directives))
  ;;
end

let module_command = Cmd.v Module.info Module.term
