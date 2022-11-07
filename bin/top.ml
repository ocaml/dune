open Stdune
open Import

let doc =
  "Print a list of toplevel directives for including directories and loading \
   cma files."

let man =
  [ `S "DESCRIPTION"
  ; `P
      {|Print a list of toplevel directives for including directories and loading cma files.|}
  ; `P
      {|The output of $(b,dune toplevel-init-file) should be evaluated in a toplevel
          to make a library available there.|}
  ; `Blocks Common.help_secs
  ]

let info = Cmd.info "top" ~doc ~man

let link_deps sctx link =
  let open Memo.O in
  Memo.parallel_map link ~f:(fun t ->
      Dune_rules.Lib_flags.link_deps sctx t Dune_rules.Link_mode.Byte)
  >>| List.concat

let files_to_load_of_requires sctx requires =
  let open Memo.O in
  let* files = link_deps sctx requires in
  let+ () = Memo.parallel_iter files ~f:Build_system.build_file in
  List.filter files ~f:(fun p ->
      let ext = Path.extension p in
      ext = Ocaml.Mode.compiled_lib_ext Byte || ext = Ocaml.Cm_kind.ext Cmo)

let term =
  let+ common = Common.term
  and+ dir = Arg.(value & pos 0 string "" & Arg.info [] ~docv:"DIR")
  and+ ctx_name =
    Common.context_arg ~doc:{|Select context where to build/run utop.|}
  in
  let config = Common.init common in
  Scheduler.go ~common ~config (fun () ->
      let open Fiber.O in
      let* setup = Import.Main.setup () in
      Build_system.run_exn (fun () ->
          let open Memo.O in
          let* setup = setup in
          let sctx =
            Dune_engine.Context_name.Map.find setup.scontexts ctx_name
            |> Option.value_exn
          in
          let* libs =
            let dir =
              let build_dir = (Super_context.context sctx).build_dir in
              Path.Build.relative build_dir (Common.prefix_target common dir)
            in
            let* db =
              let+ scope = Dune_rules.Scope.DB.find_by_dir dir in
              Dune_rules.Scope.libs scope
            in
            Dune_rules.Utop.libs_under_dir sctx ~db ~dir:(Path.build dir)
          in
          let* requires =
            Dune_rules.Resolve.Memo.read_memo
              (Dune_rules.Lib.closure ~linking:true libs)
          in
          let include_paths =
            Dune_rules.Lib_flags.L.toplevel_include_paths requires
          in
          let+ files_to_load = files_to_load_of_requires sctx requires in
          Dune_rules.Toplevel.print_toplevel_init_file ~include_paths
            ~files_to_load ~uses:[] ~pp:None ~ppx:None))

let command = Cmd.v info term

module Module = struct
  let doc =
    "Print a list of toplevel directives for loading a module into the topevel."

  let man =
    [ `S "DESCRIPTION"
    ; `P doc
    ; `P
        "The module's source is evaluated in the toplevel without being sealed \
         by the mli."
    ; `P
        {|The output of $(b,dune toplevel-init-file) should be evaluated in a toplevel
          to make the module available there.|}
    ; `Blocks Common.help_secs
    ]

  let info = Cmd.info "top-module" ~doc ~man

  let module_directives sctx mod_ =
    let ctx = Super_context.context sctx in
    let src = Path.Build.append_source ctx.build_dir mod_ in
    let dir = Path.Build.parent_exn src in
    let open Memo.O in
    let module_name =
      let name = src |> Path.Build.basename |> Filename.chop_extension in
      match Dune_rules.Module_name.of_string_user_error (Loc.none, name) with
      | Ok s -> s
      | Error e -> raise (User_error.E e)
    in
    let drop_rules f =
      let+ res, _ =
        Memo.Implicit_output.collect Dune_engine.Rules.implicit_output f
      in
      res
    in
    let* dir_contents =
      drop_rules @@ fun () -> Dune_rules.Dir_contents.get sctx ~dir
    in
    let* ocaml = Dune_rules.Dir_contents.ocaml dir_contents in
    let stanza =
      match Dune_rules.Ml_sources.find_origin ocaml module_name with
      | None -> User_error.raise [ Pp.text "stanza not found for module" ]
      | Some m -> m
    in
    let* scope = Dune_rules.Scope.DB.find_by_dir dir in
    let* expander = Super_context.expander sctx ~dir in
    let stanza =
      match stanza with
      | Executables exes -> `Executables exes
      | Library lib -> `Library lib
      | Melange _ ->
        User_error.raise
          [ Pp.text "melange modules cannot be loaded into toplevel" ]
    in
    let* cctx, merlin =
      drop_rules @@ fun () ->
      match stanza with
      | `Executables exes ->
        Dune_rules.Exe_rules.rules ~sctx ~dir ~dir_contents ~scope ~expander
          exes
      | `Library lib ->
        Dune_rules.Lib_rules.rules lib ~sctx ~dir_contents ~dir ~expander ~scope
    in
    let modules = Dune_rules.Compilation_context.modules cctx in
    let module_ =
      match Dune_rules.Modules.find modules module_name with
      | Some m -> m
      | None -> User_error.raise [ Pp.textf "module not found" ]
    in
    let obj_dir = Dune_rules.Compilation_context.obj_dir cctx in
    let* compile =
      match stanza with
      | `Executables exes -> Dune_rules.Exe_rules.compile_info ~scope exes
      | `Library lib ->
        let libs = Dune_rules.Scope.libs scope in
        let+ _, compile =
          Dune_rules.Lib.DB.get_compile_info libs
            (Dune_rules.Dune_file.Library.best_name lib)
            ~allow_overlaps:lib.buildable.allow_overlapping_dependencies
        in
        compile
    in
    let* requires =
      let* requires =
        Dune_rules.Lib.Compile.requires_link compile |> Memo.Lazy.force
      in
      Dune_rules.Resolve.read_memo requires
    in
    let private_cm_dir =
      let key =
        ( Dune_rules.Module.source module_ ~ml_kind:Impl
        , match stanza with
          | `Executables exes -> `Executables exes.names
          | `Library lib -> `Library lib.name )
      in
      let key = String.take (Digest.generic key |> Digest.to_string) 12 in
      Path.Build.(relative root)
        (sprintf ".top/%s.%s"
           (Dune_rules.Module_name.to_string module_name)
           key)
    in
    let include_paths =
      let libs = Dune_rules.Lib_flags.L.toplevel_include_paths requires in
      Path.Set.add libs (Path.build private_cm_dir)
    in
    let source_deps =
      (* We this explicit dep for two reasons:

         - to copy the source file to the build dir if ocamldep doesn't require
         it to compute the deps (single module exes for example)

         - To force building the preprocessor so that the repl can use it. It's
         kind of a hack since we don't need to preprocess the source, but it's
         the easiest way to do it
      *)
      match Dune_rules.Module.file module_ ~ml_kind:Impl with
      | None -> User_error.raise [ Pp.textf "cannot loads mli only module" ]
      | Some f -> fun () -> Build_system.build_file f
    in
    let files_to_load () =
      let+ libs, modules =
        Memo.fork_and_join
          (fun () -> files_to_load_of_requires sctx requires)
          (fun () ->
            let dep_graph =
              let dg = Dune_rules.Compilation_context.dep_graphs cctx in
              Ocaml.Ml_kind.Dict.get dg Impl
            in
            let* module_deps =
              let action = Dune_rules.Dep_graph.deps_of dep_graph module_ in
              let+ graph, _ = Action_builder.run action Eager in
              graph
            in
            let files =
              List.filter_map module_deps ~f:(fun module_ ->
                  Dune_rules.Obj_dir.Module.cm_file obj_dir module_
                    ~kind:(Ocaml Cmo)
                  |> Option.map ~f:Path.build)
            in
            let+ () = Memo.parallel_iter files ~f:Build_system.build_file in
            (* now we copy these files to a special temp dir to prevent the
               toplevel from peaking where it shouldn't

               if the copmiler ever supports pointing at individual cmi's, these hacks
               will not be needed. *)
            Path.mkdir_p (Path.build private_cm_dir);
            let (_ : Fpath.clear_dir_result) =
              Path.clear_dir (Path.build private_cm_dir)
            in
            let copy =
              if Sys.win32 then fun ~src ~dst -> Io.copy_file ~src ~dst ()
              else fun ~src ~dst -> Path.link src dst
            in
            List.iter module_deps ~f:(fun module_ ->
                let file =
                  Dune_rules.Obj_dir.Module.cm_file_exn obj_dir module_
                    ~kind:(Ocaml Cmi)
                in
                let new_file =
                  Path.Build.relative private_cm_dir (Path.Build.basename file)
                in
                copy ~src:(Path.build file) ~dst:(Path.build new_file));
            files)
      in
      libs @ modules
    in
    let pps () =
      let module Merlin = Dune_rules.Merlin in
      let pps = Merlin.pp_config merlin sctx ~expander in
      let+ pps, _ = Action_builder.run pps Eager in
      let pp = Dune_rules.Module_name.Per_item.get pps module_name in
      match pp with
      | None -> (None, None)
      | Some pp_flags -> (
        let args = Merlin.Processed.pp_args pp_flags in
        match Merlin.Processed.pp_kind pp_flags with
        | Pp -> (Some args, None)
        | Ppx -> (None, Some args))
    in
    let+ (pp, ppx), files_to_load =
      Memo.fork_and_join pps (fun () ->
          Memo.fork_and_join_unit source_deps files_to_load)
    in
    (include_paths, files_to_load, src, pp, ppx)

  let term =
    let+ common = Common.term
    and+ module_path =
      Arg.(value & pos 0 string "" & Arg.info [] ~docv:"MODULE")
    and+ ctx_name =
      Common.context_arg ~doc:{|Select context where to build/run utop.|}
    in
    let config = Common.init common in
    Scheduler.go ~common ~config (fun () ->
        let open Fiber.O in
        let* setup = Import.Main.setup () in
        Build_system.run_exn (fun () ->
            let open Memo.O in
            let* setup = setup in
            let sctx =
              Dune_engine.Context_name.Map.find setup.scontexts ctx_name
              |> Option.value_exn
            in
            let+ include_paths, files_to_load, use, pp, ppx =
              let module_path =
                let root = Common.root common in
                Path.Source.relative Path.Source.root
                  (root.reach_from_root_prefix ^ module_path)
              in
              module_directives sctx module_path
            in
            Dune_rules.Toplevel.print_toplevel_init_file ~include_paths
              ~files_to_load
              ~uses:[ Path.build use ]
              ~pp ~ppx))
end

let module_command = Cmd.v Module.info Module.term
