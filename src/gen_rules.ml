open Import
module Menhir_rules = Menhir
open Jbuild
open Build.O
open! No_io

(* +-----------------------------------------------------------------+
   | Utils                                                           |
   +-----------------------------------------------------------------+ *)

module Gen(P : Install_rules.Params) = struct
  module Alias = Build_system.Alias
  module CC = Compilation_context
  module SC = Super_context
  module Odoc = Odoc.Gen(P)

  let sctx = P.sctx
  let ctx = SC.context sctx

  let opaque =
    ctx.profile = "dev" && Ocaml_version.supports_opaque_for_mli ctx.version

  (* +-----------------------------------------------------------------+
     | Library stuff                                                   |
     +-----------------------------------------------------------------+ *)

  let msvc_hack_cclibs cclibs =
    let f lib =
      if String.is_prefix lib ~prefix:"-l" then
        String.sub lib ~pos:2 ~len:(String.length lib - 2) ^ ".lib"
      else
        lib
    in
    let cclibs = List.map cclibs ~f in
    let f lib =
      if String.is_prefix lib ~prefix:"-l" then
        String.sub lib ~pos:2 ~len:(String.length lib - 2)
      else
        lib
    in
    List.map cclibs ~f

  let build_lib (lib : Library.t) ~scope ~flags ~dir ~obj_dir ~mode
        ~top_sorted_modules ~modules =
    Option.iter (Context.compiler ctx mode) ~f:(fun compiler ->
      let target = Library.archive lib ~dir ~ext:(Mode.compiled_lib_ext mode) in
      let stubs_flags =
        if not (Library.has_stubs lib) then
          []
        else
          let stubs_name = lib.name ^ "_stubs" in
          match mode with
          | Byte -> ["-dllib"; "-l" ^ stubs_name; "-cclib"; "-l" ^ stubs_name]
          | Native -> ["-cclib"; "-l" ^ stubs_name]
      in
      let map_cclibs =
        (* https://github.com/ocaml/dune/issues/119 *)
        if ctx.ccomp_type = "msvc" then
          msvc_hack_cclibs
        else
          fun x -> x
      in
      let artifacts ~ext modules =
        List.map modules ~f:(Module.obj_file ~obj_dir ~ext)
      in
      let obj_deps =
        Build.paths (artifacts modules ~ext:(Cm_kind.ext (Mode.cm_kind mode)))
      in
      let obj_deps =
        match mode with
        | Byte   -> obj_deps
        | Native ->
          obj_deps >>>
          Build.paths (artifacts modules ~ext:ctx.ext_obj)
      in
      SC.add_rule sctx
        (obj_deps
         >>>
         Build.fanout4
           (top_sorted_modules >>^artifacts ~ext:(Cm_kind.ext (Mode.cm_kind mode)))
           (SC.expand_and_eval_set sctx ~scope ~dir lib.c_library_flags
              ~standard:(Build.return []))
           (Ocaml_flags.get flags mode)
           (SC.expand_and_eval_set sctx ~scope ~dir lib.library_flags
              ~standard:(Build.return []))
         >>>
         Build.run ~context:ctx (Ok compiler)
           [ Dyn (fun (_, _, flags, _) -> As flags)
           ; A "-a"; A "-o"; Target target
           ; As stubs_flags
           ; Dyn (fun (_, cclibs, _, _) -> Arg_spec.quote_args "-cclib" (map_cclibs cclibs))
           ; Dyn (fun (_, _, _, library_flags) -> As library_flags)
           ; As (match lib.kind with
               | Normal -> []
               | Ppx_deriver | Ppx_rewriter -> ["-linkall"])
           ; Dyn (fun (cm_files, _, _, _) -> Deps cm_files)
           ; Hidden_targets
               (match mode with
                | Byte -> []
                | Native -> [Library.archive lib ~dir ~ext:ctx.ext_lib])
           ]))

  let build_c_file (lib : Library.t) ~scope ~dir ~includes (src, dst) =
    SC.add_rule sctx
      (SC.expand_and_eval_set sctx ~scope ~dir lib.c_flags
         ~standard:(Build.return (Context.cc_g ctx))
       >>>
       Build.run ~context:ctx
         (* We have to execute the rule in the library directory as
            the .o is produced in the current directory *)
         ~dir:(Path.parent_exn src)
         (Ok ctx.ocamlc)
         [ As (Utils.g ())
         ; includes
         ; Dyn (fun c_flags -> Arg_spec.quote_args "-ccopt" c_flags)
         ; A "-o"; Target dst
         ; Dep src
         ]);
    dst

  let build_cxx_file (lib : Library.t) ~scope ~dir ~includes (src, dst) =
    let open Arg_spec in
    let output_param =
      if ctx.ccomp_type = "msvc" then
        [Concat ("", [A "/Fo"; Target dst])]
      else
        [A "-o"; Target dst]
    in
    SC.add_rule sctx
      (SC.expand_and_eval_set sctx ~scope ~dir lib.cxx_flags
         ~standard:(Build.return (Context.cc_g ctx))
       >>>
       Build.run ~context:ctx
         (* We have to execute the rule in the library directory as
            the .o is produced in the current directory *)
         ~dir:(Path.parent_exn src)
         (SC.resolve_program ~loc:None sctx ctx.c_compiler)
         ([ S [A "-I"; Path ctx.stdlib_dir]
          ; As (SC.cxx_flags sctx)
          ; includes
          ; Dyn (fun cxx_flags -> As cxx_flags)
          ] @ output_param @
          [ A "-c"; Dep src
          ]));
    dst

  (* If the compiler reads the cmi for module alias even with
     [-w -49 -no-alias-deps], we must sandbox the build of the
     alias module since the modules it references are built after. *)
  let alias_module_build_sandbox =
    Ocaml_version.always_reads_alias_cmi ctx.version

  let library_rules (lib : Library.t) ~dir_contents ~dir ~scope
        ~compile_info ~dir_kind =
    let obj_dir = Utils.library_object_directory ~dir lib.name in
    let requires = Lib.Compile.requires compile_info in
    let dep_kind =
      if lib.optional then Lib_deps_info.Kind.Optional else Required
    in
    let flags = SC.ocaml_flags sctx ~scope ~dir lib.buildable in
    let { Dir_contents.Library_modules.
          modules; main_module_name; alias_module } =
      Dir_contents.modules_of_library dir_contents ~name:(Library.best_name lib)
    in
    let source_modules = modules in
    (* Preprocess before adding the alias module as it doesn't need
       preprocessing *)
    let pp =
      Preprocessing.make sctx ~dir ~dep_kind ~scope
        ~preprocess:lib.buildable.preprocess
        ~preprocessor_deps:
          (SC.Deps.interpret sctx ~scope ~dir
             lib.buildable.preprocessor_deps)
        ~lint:lib.buildable.lint
        ~lib_name:(Some lib.name)
        ~dir_kind
    in
    let modules = Preprocessing.pp_modules pp modules in

    let modules =
      match alias_module with
      | None -> modules
      | Some m -> Module.Name.Map.add modules m.name m
    in

    let lib_interface_module =
      if lib.wrapped then
        Module.Name.Map.find modules main_module_name
      else
        None
    in
    let cctx =
      Compilation_context.create ()
        ~super_context:sctx
        ~scope
        ~dir
        ~dir_kind
        ~obj_dir
        ~modules
        ?alias_module
        ?lib_interface_module
        ~flags
        ~requires
        ~preprocessing:pp
        ~no_keep_locs:lib.no_keep_locs
        ~opaque
    in

    let dep_graphs = Ocamldep.rules cctx in

    Option.iter alias_module ~f:(fun m ->
      let file =
        match m.impl with
        | Some f -> f
        | None -> Option.value_exn m.intf
      in
      SC.add_rule sctx
        (Build.return
           (Module.Name.Map.values (Module.Name.Map.remove modules m.name)
            |> List.map ~f:(fun (m : Module.t) ->
              sprintf "(** @canonical %s.%s *)\n\
                       module %s = %s\n"
                (Module.Name.to_string main_module_name)
                (Module.Name.to_string m.name)
                (Module.Name.to_string m.name)
                (Module.Name.to_string (Module.real_unit_name m))
            )
            |> String.concat ~sep:"\n")
         >>> Build.write_file_dyn file.path));


    let dynlink = lib.dynlink in
    let js_of_ocaml = lib.buildable.js_of_ocaml in
    Module_compilation.build_modules cctx ~js_of_ocaml ~dynlink ~dep_graphs;
    Option.iter alias_module ~f:(fun m ->
      let cctx = Compilation_context.for_alias_module cctx in
      Module_compilation.build_module cctx m
        ~js_of_ocaml
        ~dynlink
        ~sandbox:alias_module_build_sandbox
        ~dep_graphs:(Ocamldep.Dep_graphs.dummy m));

    if Library.has_stubs lib then begin
      let all_dirs = Dir_contents.dirs dir_contents in
      let h_files =
        List.fold_left all_dirs ~init:[] ~f:(fun acc dc ->
          String.Set.fold (Dir_contents.text_files dc) ~init:acc
            ~f:(fun fn acc ->
              if String.is_suffix fn ~suffix:".h" then
                Path.relative (Dir_contents.dir dc) fn :: acc
              else
                acc))
      in
      let all_dirs = Path.Set.of_list (List.map all_dirs ~f:Dir_contents.dir) in
      let resolve_name ~ext (loc, fn) =
        let p = Path.relative dir (fn ^ ext) in
        if not (match Path.parent p with
          | None -> false
          | Some p -> Path.Set.mem all_dirs p) then
          Loc.fail loc
            "File %a is not part of the current directory group. \
             This is not allowed."
            Path.pp (Path.drop_optional_build_context p)
        ;
        (p, Path.relative dir (fn ^ ctx.ext_obj))
      in
      let o_files =
        let includes =
          Arg_spec.S
            [ Hidden_deps h_files
            ; Arg_spec.of_result_map requires ~f:(fun libs ->
                S [ Lib.L.c_include_flags libs ~stdlib_dir:ctx.stdlib_dir
                  ; Hidden_deps (SC.Libs.file_deps sctx libs ~ext:".h")
                  ])
            ]
        in
        List.map lib.c_names ~f:(fun name ->
          build_c_file   lib ~scope ~dir ~includes (resolve_name name ~ext:".c")
        ) @ List.map lib.cxx_names ~f:(fun name ->
          build_cxx_file lib ~scope ~dir ~includes (resolve_name name ~ext:".cpp")
        )
      in
      match lib.self_build_stubs_archive with
      | Some _ -> ()
      | None ->
        let ocamlmklib ~sandbox ~custom ~targets =
          SC.add_rule sctx ~sandbox
            (SC.expand_and_eval_set sctx ~scope ~dir
               lib.c_library_flags ~standard:(Build.return [])
             >>>
             Build.run ~context:ctx
               (Ok ctx.ocamlmklib)
               [ As (Utils.g ())
               ; if custom then A "-custom" else As []
               ; A "-o"
               ; Path (Path.relative dir (sprintf "%s_stubs" lib.name))
               ; Deps o_files
               ; Dyn (fun cclibs ->
                   (* https://github.com/ocaml/dune/issues/119 *)
                   if ctx.ccomp_type = "msvc" then
                     let cclibs = msvc_hack_cclibs cclibs in
                     Arg_spec.quote_args "-ldopt" cclibs
                   else
                     As cclibs
                 )
               ; Hidden_targets targets
               ])
        in
        let static = Library.stubs_archive lib ~dir ~ext_lib:ctx.ext_lib in
        let dynamic = Library.dll lib ~dir ~ext_dll:ctx.ext_dll in
        let modes =
          Mode_conf.Set.eval lib.modes
            ~has_native:(Option.is_some ctx.ocamlopt)
        in
        if modes.native &&
           modes.byte   &&
           lib.dynlink
        then begin
          (* If we build for both modes and support dynlink, use a
             single invocation to build both the static and dynamic
             libraries *)
          ocamlmklib ~sandbox:false ~custom:false ~targets:[static; dynamic]
        end else begin
          ocamlmklib ~sandbox:false ~custom:true ~targets:[static];
          (* We can't tell ocamlmklib to build only the dll, so we
             sandbox the action to avoid overriding the static archive *)
          ocamlmklib ~sandbox:true ~custom:false ~targets:[dynamic]
        end
    end;

    List.iter Cm_kind.all ~f:(fun cm_kind ->
      let files =
        Module.Name.Map.fold modules ~init:Path.Set.empty ~f:(fun m acc ->
          match Module.cm_file m ~obj_dir cm_kind with
          | None -> acc
          | Some fn -> Path.Set.add acc fn)
      in
      SC.Libs.setup_file_deps_alias sctx ~dir lib ~ext:(Cm_kind.ext cm_kind)
        files);
    SC.Libs.setup_file_deps_group_alias sctx ~dir lib ~exts:[".cmi"; ".cmx"];
    SC.Libs.setup_file_deps_alias sctx ~dir lib ~ext:".h"
      (List.map lib.install_c_headers ~f:(fun header ->
         Path.relative dir (header ^ ".h"))
       |> Path.Set.of_list);

    (let modules =
       Module.Name.Map.fold modules ~init:[] ~f:(fun m acc ->
         if Module.has_impl m then
           m :: acc
         else
           acc)
     in
     let top_sorted_modules =
       Ocamldep.Dep_graph.top_closed_implementations dep_graphs.impl modules
     in
     List.iter Mode.all ~f:(fun mode ->
       build_lib lib ~scope ~flags ~dir ~obj_dir ~mode ~top_sorted_modules
         ~modules));
    (* Build *.cma.js *)
    SC.add_rules sctx (
      let src = Library.archive lib ~dir ~ext:(Mode.compiled_lib_ext Mode.Byte) in
      let target = Path.extend_basename src ~suffix:".js" in
      Js_of_ocaml_rules.build_cm cctx
        ~js_of_ocaml:lib.buildable.js_of_ocaml ~src ~target);

    if ctx.natdynlink_supported then
      Option.iter ctx.ocamlopt ~f:(fun ocamlopt ->
        let src = Library.archive lib ~dir ~ext:(Mode.compiled_lib_ext Native) in
        let dst = Library.archive lib ~dir ~ext:".cmxs" in
        let build =
          Build.dyn_paths (Build.arr (fun () ->
            [Library.archive lib ~dir ~ext:ctx.ext_lib]))
          >>>
          Ocaml_flags.get flags Native
          >>>
          Build.run ~context:ctx
            (Ok ocamlopt)
            [ Dyn (fun flags -> As flags)
            ; A "-shared"; A "-linkall"
            ; A "-I"; Path dir
            ; A "-o"; Target dst
            ; Dep src
            ]
        in
        let build =
          if Library.has_stubs lib then
            Build.path (Library.stubs_archive ~dir lib ~ext_lib:ctx.ext_lib)
            >>>
            build
          else
            build
        in
        SC.add_rule sctx build
      );

    Odoc.setup_library_odoc_rules lib ~requires ~modules ~dep_graphs ~scope;

    let flags =
      match alias_module with
      | None -> Ocaml_flags.common flags
      | Some m ->
        Ocaml_flags.prepend_common ["-open"; Module.Name.to_string m.name] flags
        |> Ocaml_flags.common
    in

    Sub_system.gen_rules
      { super_context = sctx
      ; dir
      ; stanza = lib
      ; scope
      ; source_modules
      ; compile_info
      };

    (cctx,
     Merlin.make ()
       ~requires:(Lib.Compile.requires compile_info)
       ~flags
       ~preprocess:(Buildable.single_preprocess lib.buildable)
       ~libname:lib.name
       ~objs_dirs:(Path.Set.singleton obj_dir))

  let library_rules (lib : Library.t) ~dir_contents ~dir ~scope
        ~dir_kind : Compilation_context.t * Merlin.t =
    let compile_info =
      Lib.DB.get_compile_info (Scope.libs scope) lib.name
        ~allow_overlaps:lib.buildable.allow_overlapping_dependencies
    in
    SC.Libs.gen_select_rules sctx compile_info ~dir;
    SC.Libs.with_lib_deps sctx compile_info ~dir
      ~f:(fun () ->
        library_rules lib ~dir_contents ~dir ~scope ~compile_info
          ~dir_kind)

  (* +-----------------------------------------------------------------+
     | Executables stuff                                               |
     +-----------------------------------------------------------------+ *)

  let executables_rules ~dir ~dir_kind
        ~dir_contents ~scope ~compile_info
        (exes : Executables.t) =
    (* Use "eobjs" rather than "objs" to avoid a potential conflict
       with a library of the same name *)
    let obj_dir =
      Utils.executable_object_directory ~dir (List.hd exes.names |> snd)
    in
    let requires = Lib.Compile.requires compile_info in
    let modules =
      Dir_contents.modules_of_executables dir_contents
        ~first_exe:(snd (List.hd exes.names))
    in

    let preprocessor_deps =
      SC.Deps.interpret sctx exes.buildable.preprocessor_deps
        ~scope ~dir
    in
    let pp =
      Preprocessing.make sctx ~dir ~dep_kind:Required
        ~scope
        ~preprocess:exes.buildable.preprocess
        ~preprocessor_deps
        ~lint:exes.buildable.lint
        ~lib_name:None
        ~dir_kind
    in
    let modules =
      Module.Name.Map.map modules ~f:(fun m ->
        Preprocessing.pp_module_as pp m.name m)
    in

    let programs =
      List.map exes.names ~f:(fun (loc, name) ->
        let mod_name = Module.Name.of_string name in
        match Module.Name.Map.find modules mod_name with
        | Some m ->
          if not (Module.has_impl m) then
            Loc.fail loc "Module %a has no implementation."
              Module.Name.pp mod_name
          else
            { Exe.Program.name; main_module_name = mod_name }
        | None -> Loc.fail loc "Module %a doesn't exist."
                    Module.Name.pp mod_name)
    in

    let linkages =
      let module L = Executables.Link_mode in
      let l =
        let has_native = Option.is_some ctx.ocamlopt in
        List.filter_map (L.Set.to_list exes.modes) ~f:(fun (mode : L.t) ->
          if not has_native && mode.mode = Native then
            None
          else
            Some (Exe.Linkage.of_user_config ctx mode))
      in
      (* If bytecode was requested but not native or best version,
         add custom linking *)
      if L.Set.mem exes.modes L.byte         &&
         not (L.Set.mem exes.modes L.native) &&
         not (L.Set.mem exes.modes L.exe) then
        Exe.Linkage.custom :: l
      else
        l
    in

    let flags = SC.ocaml_flags sctx ~scope ~dir exes.buildable in
    let link_deps =
      SC.Deps.interpret sctx ~scope ~dir exes.link_deps
    in
    let link_flags =
      link_deps >>^ ignore >>>
      SC.expand_and_eval_set sctx exes.link_flags
        ~scope
        ~dir
        ~standard:(Build.return [])
    in

    let cctx =
      Compilation_context.create ()
        ~super_context:sctx
        ~scope
        ~dir
        ~dir_kind
        ~obj_dir
        ~modules
        ~flags
        ~requires
        ~preprocessing:pp
        ~opaque
    in

    Exe.build_and_link_many cctx
      ~programs
      ~linkages
      ~link_flags
      ~js_of_ocaml:exes.buildable.js_of_ocaml;

    (cctx,
     Merlin.make ()
       ~requires:(Lib.Compile.requires compile_info)
       ~flags:(Ocaml_flags.common flags)
       ~preprocess:(Buildable.single_preprocess exes.buildable)
       ~objs_dirs:(Path.Set.singleton obj_dir))

  let executables_rules ~dir
        ~dir_contents ~scope ~dir_kind
        (exes : Executables.t) : Compilation_context.t * Merlin.t =
    let compile_info =
      Lib.DB.resolve_user_written_deps (Scope.libs scope)
        exes.buildable.libraries
        ~pps:(Jbuild.Preprocess_map.pps exes.buildable.preprocess)
        ~allow_overlaps:exes.buildable.allow_overlapping_dependencies
    in
    SC.Libs.gen_select_rules sctx compile_info ~dir;
    SC.Libs.with_lib_deps sctx compile_info ~dir
      ~f:(fun () ->
        executables_rules exes ~dir
          ~dir_contents ~scope ~compile_info ~dir_kind)

  (* +-----------------------------------------------------------------+
     | Tests                                                           |
     +-----------------------------------------------------------------+ *)

  let tests_rules (t : Tests.t) ~dir ~scope ~dir_contents
        ~dir_kind ~src_dir =
    let test_kind (loc, name) =
      let sources = SC.source_files sctx ~src_path:src_dir in
      let expected_basename = name ^ ".expected" in
      if String.Set.mem sources expected_basename then
        `Expect
          { Action.Unexpanded.Diff.
            file1 = String_with_vars.make_text loc expected_basename
          ; file2 = String_with_vars.make_text loc (name ^ ".output")
          ; optional = false
          ; mode = Text
          }
      else
        `Regular
    in
    let regular_rule run_action alias loc =
      { alias with Alias_conf.action = Some (loc, run_action) }
    in
    let expect_rule run_action (diff : Action.Unexpanded.Diff.t) alias loc =
      let rule =
        { Rule.
          targets = Infer
        ; deps = Bindings.empty
        ; action =
            (loc, Action.Unexpanded.Redirect (Stdout, diff.file2, run_action))
        ; mode = Standard
        ; locks = t.locks
        ; loc
        } in
      let alias =
        { alias with
          Alias_conf.
          action = Some (loc, Diff diff)
        ; locks = t.locks
        } in
      (alias, rule)
    in
    List.iter t.exes.names ~f:(fun (loc, s) ->
      let run_action =
        Action.Unexpanded.Run
          (String_with_vars.make_text loc ("./" ^ s ^ ".exe"), []) in
      let base_alias =
        { Alias_conf.
          name = "runtest"
        ; locks = []
        ; package = t.package
        ; deps = t.deps
        ; action = None
        ; enabled_if = t.enabled_if
        ; loc
        } in
      match test_kind (loc, s) with
      | `Regular ->
        Simple_rules.alias sctx ~dir ~scope
          (regular_rule run_action base_alias loc)
      | `Expect diff ->
        let (alias, rule) =
          expect_rule run_action diff base_alias loc in
        Simple_rules.alias sctx alias ~dir ~scope;
        ignore (Simple_rules.user_rule sctx rule ~dir ~scope : Path.t list));
    executables_rules t.exes ~dir ~scope ~dir_kind
      ~dir_contents

  (* +-----------------------------------------------------------------+
     | Stanza                                                          |
     +-----------------------------------------------------------------+ *)

  let gen_rules dir_contents
        { SC.Dir_with_jbuild. src_dir; ctx_dir; stanzas; scope; kind } =
    let merlins, cctxs =
      let rec loop stanzas merlins cctxs =
        let dir = ctx_dir in
        match stanzas with
        | [] -> (List.rev merlins, List.rev cctxs)
        | stanza :: stanzas ->
          match (stanza : Stanza.t) with
          | Library lib ->
            let cctx, merlin =
              library_rules lib ~dir ~scope ~dir_contents
                ~dir_kind:kind
            in
            loop stanzas (merlin :: merlins)
              ((lib.buildable.loc, cctx) :: cctxs)
          | Executables exes ->
            let cctx, merlin =
              executables_rules exes ~dir ~scope
                ~dir_contents ~dir_kind:kind
            in
            loop stanzas (merlin :: merlins)
              ((exes.buildable.loc, cctx) :: cctxs)
          | Alias alias ->
            Simple_rules.alias sctx alias ~dir ~scope;
            loop stanzas merlins cctxs
          | Tests tests ->
            let cctx, merlin =
              tests_rules tests ~dir ~scope ~src_dir
                ~dir_contents ~dir_kind:kind
            in
            loop stanzas (merlin :: merlins)
              ((tests.exes.buildable.loc, cctx) :: cctxs)
          | Copy_files { glob; _ } ->
            let src_dir =
              let loc = String_with_vars.loc glob in
              let src_glob = SC.expand_vars_string sctx ~dir glob ~scope in
              Path.parent_exn (Path.relative src_dir src_glob ~error_loc:loc)
            in
            let merlin =
              Merlin.make ()
                ~source_dirs:(Path.Set.singleton src_dir)
            in
            loop stanzas (merlin :: merlins) cctxs
          | _ ->
            loop stanzas merlins cctxs
      in
      loop stanzas [] []
    in
    Option.iter (Merlin.merge_all merlins) ~f:(fun m ->
      let more_src_dirs =
        List.map (Dir_contents.dirs dir_contents) ~f:(fun dc ->
          Path.drop_optional_build_context (Dir_contents.dir dc))
      in
      Merlin.add_rules sctx ~dir:ctx_dir ~more_src_dirs ~scope ~dir_kind:kind
        (Merlin.add_source_dir m src_dir));
    Utop.setup sctx ~dir:ctx_dir ~scope ~libs:(
      List.filter_map stanzas ~f:(function
        | Library lib -> Some lib
        | _ -> None));
    List.iter stanzas ~f:(fun stanza ->
      match (stanza : Stanza.t) with
      | Menhir.T m ->
        begin match
          List.find_map (Menhir_rules.module_names m)
            ~f:(fun name ->
              Option.bind (Dir_contents.lookup_module dir_contents name)
                ~f:(fun buildable ->
                  List.find_map cctxs ~f:(fun (loc, cctx) ->
                    Option.some_if (loc = buildable.loc) cctx)))
        with
        | None ->
          (* This happens often when passing a [-p ...] option that
             hides a library *)
          let targets =
            List.map (Menhir_rules.targets m) ~f:(Path.relative ctx_dir)
          in
          SC.add_rule sctx
            (Build.fail ~targets
               { fail = fun () ->
                   Loc.fail m.loc
                     "I can't determine what library/executable the files \
                      produced by this stanza are part of."
               })
        | Some cctx ->
          Menhir_rules.gen_rules cctx m
        end
      | _ -> ())

  let gen_rules dir_contents ~dir =
    match SC.stanzas_in sctx ~dir with
    | None -> ()
    | Some d -> gen_rules dir_contents d

  let gen_rules ~dir components : Build_system.extra_sub_directories_to_keep =
    (match components with
     | ".js"  :: rest -> Js_of_ocaml_rules.setup_separate_compilation_rules
                           sctx rest
     | "_doc" :: rest -> Odoc.gen_rules rest ~dir
     | ".ppx"  :: rest -> Preprocessing.gen_rules sctx rest
     | _ ->
       match
         File_tree.find_dir (SC.file_tree sctx)
           (Path.drop_build_context_exn dir)
       with
       | None ->
         (* We get here when [dir] is a generated directory, such as
            [.utop] or [.foo.objs]. *)
         if components <> [] then SC.load_dir sctx ~dir:(Path.parent_exn dir)
       | Some _ ->
         (* This interprets "rule" and "copy_files" stanzas. *)
         let dir_contents = Dir_contents.get sctx ~dir in
         match Dir_contents.kind dir_contents with
         | Standalone ->
           gen_rules dir_contents ~dir
         | Group_part root ->
           SC.load_dir sctx ~dir:(Dir_contents.dir root)
         | Group_root (lazy subs) ->
           gen_rules dir_contents ~dir;
           List.iter subs ~f:(fun dc ->
             gen_rules dir_contents ~dir:(Dir_contents.dir dc)));
    match components with
    | [] -> These (String.Set.of_list [".js"; "_doc"; ".ppx"])
    | [(".js"|"_doc"|".ppx")] -> All
    | _  -> These String.Set.empty

  let init () =
    let module Install_rules =
      Install_rules.Gen(P)
    in
    Install_rules.init ();
    Odoc.init ()
end

module type Gen = sig
  val gen_rules
    :  dir:Path.t
    -> string list
    -> Build_system.extra_sub_directories_to_keep
  val init : unit -> unit
  val sctx : Super_context.t
end

let gen ~contexts ~build_system
      ?(external_lib_deps_mode=false)
      ?only_packages conf =
  let open Fiber.O in
  let { Jbuild_load. file_tree; jbuilds; packages; projects } = conf in
  let packages =
    match only_packages with
    | None -> packages
    | Some pkgs ->
      Package.Name.Map.filter packages ~f:(fun { Package.name; _ } ->
        Package.Name.Set.mem pkgs name)
  in
  let sctxs = Hashtbl.create 4 in
  List.iter contexts ~f:(fun c ->
    Hashtbl.add sctxs c.Context.name (Fiber.Ivar.create ()));
  let make_sctx (context : Context.t) : _ Fiber.t =
    let host () =
      match context.for_host with
      | None -> Fiber.return None
      | Some h ->
        Fiber.Ivar.read (Option.value_exn (Hashtbl.find sctxs h.name))
        >>| fun x -> Some x
    in
    let stanzas () =
      Jbuild_load.Jbuilds.eval ~context jbuilds >>| fun stanzas ->
      match only_packages with
      | None -> stanzas
      | Some pkgs ->
        List.map stanzas ~f:(fun (dir_conf : Jbuild_load.Jbuild.t) ->
          let stanzas =
            List.filter dir_conf.stanzas ~f:(fun stanza ->
              match (stanza : Stanza.t) with
              | Library { public = Some { package; _ }; _ }
              | Alias { package = Some package ;  _ }
              | Install { package; _ }
              | Documentation { package; _ } ->
                Package.Name.Set.mem pkgs package.name
              | _ -> true)
          in
          { dir_conf with stanzas })
    in
    Fiber.fork_and_join host stanzas >>= fun (host, stanzas) ->
    let sctx =
      Super_context.create
        ?host
        ~build_system
        ~context
        ~projects
        ~file_tree
        ~packages
        ~external_lib_deps_mode
        ~stanzas
    in
    let module M = Gen(struct let sctx = sctx end) in
    Fiber.Ivar.fill (Option.value_exn (Hashtbl.find sctxs context.name)) sctx
    >>| fun () ->
    (context.name, (module M : Gen))
  in
  Fiber.parallel_map contexts ~f:make_sctx >>| fun l ->
  let map = String.Map.of_list_exn l in
  Build_system.set_rule_generators build_system
    (String.Map.map map ~f:(fun (module M : Gen) -> M.gen_rules));
  String.Map.iter map ~f:(fun (module M : Gen) -> M.init ());
  String.Map.map map ~f:(fun (module M : Gen) -> M.sctx)
