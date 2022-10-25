open Import
open Memo.O

let first_exe (exes : Executables.t) = snd (Nonempty_list.hd exes.names)

let linkages
  ~dynamically_linked_foreign_archives
  (ocaml : Ocaml_toolchain.t)
  ~(exes : Executables.t)
  ~explicit_js_mode
  =
  let module L = Executables.Link_mode in
  let l =
    let has_native = Result.is_ok ocaml.ocamlopt in
    let modes =
      L.Map.to_list exes.modes
      |> List.map ~f:(fun (mode, loc) ->
        Exe.Linkage.of_user_config ocaml ~dynamically_linked_foreign_archives ~loc mode)
    in
    let modes =
      if has_native
      then modes
      else List.filter modes ~f:(fun x -> not (Exe.Linkage.is_native x))
    in
    let modes =
      if L.Map.mem exes.modes L.js
      then Exe.Linkage.byte_for_jsoo :: modes
      else if explicit_js_mode
      then modes
      else if L.Map.mem exes.modes L.byte
      then Exe.Linkage.js :: Exe.Linkage.byte_for_jsoo :: modes
      else modes
    in
    modes
  in
  (* If bytecode was requested but not native or best version, add custom
     linking *)
  if L.Map.mem exes.modes L.byte
     && (not (L.Map.mem exes.modes L.native))
     && not (L.Map.mem exes.modes L.exe)
  then Exe.Linkage.custom ocaml.version :: l
  else l
;;

let programs ~modules ~(exes : Executables.t) =
  List.map (Nonempty_list.to_list exes.names) ~f:(fun (loc, name) ->
    let mod_name = Module_name.of_string_allow_invalid (loc, name) in
    match Modules.With_vlib.find modules mod_name with
    | Some m ->
      if Module.has m ~ml_kind:Impl
      then { Exe.Program.name; main_module_name = mod_name; loc }
      else
        User_error.raise
          ~loc
          [ Pp.textf "Module %S has no implementation." (Module_name.to_string mod_name) ]
    | None ->
      let msg =
        match Ordered_set_lang.Unexpanded.loc exes.buildable.modules.modules with
        | None -> Pp.textf "Module %S doesn't exist." (Module_name.to_string mod_name)
        | Some _ ->
          Pp.textf
            "The name %S is not listed in the (modules) field of this stanza."
            (Module_name.to_string mod_name)
      in
      User_error.raise ~loc [ msg ])
;;

let o_files
  sctx
  ~dir
  ~expander
  ~(exes : Executables.t)
  ~linkages
  ~dir_contents
  ~requires_compile
  =
  if not (Executables.has_foreign exes)
  then Memo.return @@ Mode.Map.empty
  else (
    let what =
      if List.is_empty exes.buildable.foreign_stubs then "archives" else "stubs"
    in
    if List.exists linkages ~f:Exe.Linkage.is_byte
    then
      User_error.raise
        ~loc:exes.buildable.loc
        [ Pp.textf "Pure bytecode executables cannot contain foreign %s." what ]
        ~hints:
          [ Pp.text "If you only need to build a native executable use \"(modes exe)\"." ];
    let* foreign_sources =
      let+ foreign_sources = Dir_contents.foreign_sources dir_contents in
      let first_exe = first_exe exes in
      Foreign_sources.for_exes foreign_sources ~first_exe
    in
    let* foreign_o_files =
      let+ { Lib_config.ext_obj; _ } =
        let+ ocaml = Super_context.context sctx |> Context.ocaml in
        ocaml.lib_config
      in
      Foreign.Objects.build_paths exes.buildable.extra_objects ~ext_obj ~dir
    in
    let+ o_files =
      Foreign_rules.build_o_files
        ~sctx
        ~dir
        ~expander
        ~requires:requires_compile
        ~dir_contents
        ~foreign_sources
    in
    (* [foreign_o_files] are not mode-dependent *)
    Mode.Map.Multi.add_all o_files All foreign_o_files)
;;

let executables_rules
  ~sctx
  ~dir
  ~expander
  ~dir_contents
  ~scope
  ~compile_info
  ~embed_in_plugin_libraries
  (exes : Executables.t)
  =
  (* Use "eobjs" rather than "objs" to avoid a potential conflict with a library
     of the same name *)
  let* modules, obj_dir =
    let first_exe = first_exe exes in
    Dir_contents.ocaml dir_contents
    >>= Ml_sources.modules_and_obj_dir ~libs:(Scope.libs scope) ~for_:(Exe { first_exe })
  in
  let* () = Check_rules.add_obj_dir sctx ~obj_dir (Ocaml Byte) in
  let ctx = Super_context.context sctx in
  let* ocaml = Context.ocaml ctx in
  let project = Scope.project scope in
  let explicit_js_mode = Dune_project.explicit_js_mode project in
  let* linkages =
    let+ dynamically_linked_foreign_archives =
      Context.dynamically_linked_foreign_archives ctx
    in
    linkages ocaml ~dynamically_linked_foreign_archives ~exes ~explicit_js_mode
  in
  let* flags = Buildable_rules.ocaml_flags sctx ~dir exes.buildable.flags in
  let* modules, pp =
    let+ modules, pp =
      Buildable_rules.modules_rules
        sctx
        (Executables (exes.buildable, Nonempty_list.to_list exes.names))
        expander
        ~dir
        scope
        modules
    in
    Modules.With_vlib.modules modules, pp
  in
  let programs = programs ~modules ~exes in
  let* cctx =
    let requires_compile = Lib.Compile.direct_requires compile_info in
    let requires_link = Lib.Compile.requires_link compile_info in
    let js_of_ocaml =
      let js_of_ocaml = Js_of_ocaml.In_context.make ~dir exes.buildable.js_of_ocaml in
      if explicit_js_mode
      then Option.some_if (List.exists linkages ~f:Exe.Linkage.is_js) js_of_ocaml
      else Some js_of_ocaml
    in
    Compilation_context.create
      ()
      ~loc:exes.buildable.loc
      ~super_context:sctx
      ~scope
      ~obj_dir
      ~modules
      ~flags
      ~requires_link
      ~requires_compile
      ~preprocessing:pp
      ~js_of_ocaml
      ~opaque:Inherit_from_settings
      ~melange_package_name:None
      ~package:exes.package
  in
  let lib_config = ocaml.lib_config in
  let stdlib_dir = lib_config.stdlib_dir in
  let* requires_compile = Compilation_context.requires_compile cctx in
  let* requires_link = Compilation_context.requires_link cctx in
  let* dep_graphs =
    (* Building an archive for foreign stubs, we link the corresponding object
       files directly to improve perf. *)
    let link_deps, sandbox = Dep_conf_eval.unnamed ~expander exes.link_deps in
    let link_args =
      let use_standard_cxx_flags =
        match Dune_project.use_standard_c_and_cxx_flags project with
        | Some true -> Buildable.has_foreign_cxx exes.buildable
        | _ -> false
      in
      let open Action_builder.O in
      let link_flags =
        let* () = link_deps in
        let* link_flags =
          Action_builder.of_memo (Ocaml_flags_db.link_flags sctx ~dir exes.link_flags)
        in
        Link_flags.get ~use_standard_cxx_flags link_flags
      in
      let+ flags = link_flags
      and+ ctypes_cclib_flags =
        Ctypes_rules.ctypes_cclib_flags sctx ~expander ~buildable:exes.buildable
      in
      Command.Args.S
        [ As flags
        ; S
            (let ext_lib = lib_config.ext_lib in
             let foreign_archives = exes.buildable.foreign_archives |> List.map ~f:snd in
             (* XXX: don't these need the msvc hack being done in lib_rules? *)
             (* XXX: also the Command.quote_args being done in lib_rules? *)
             List.map foreign_archives ~f:(fun archive ->
               let lib =
                 Foreign.Archive.lib_file ~archive ~dir ~ext_lib ~mode:Mode.Select.All
               in
               Command.Args.S [ A "-cclib"; Dep (Path.build lib) ]))
          (* XXX: don't these need the msvc hack being done in lib_rules? *)
          (* XXX: also the Command.quote_args being done in lib_rules? *)
        ; As (List.concat_map ctypes_cclib_flags ~f:(fun f -> [ "-cclib"; f ]))
        ]
    in
    let* o_files =
      o_files sctx ~dir ~expander ~exes ~linkages ~dir_contents ~requires_compile
    in
    let* () = Check_rules.add_files sctx ~dir @@ Mode.Map.Multi.to_flat_list o_files in
    let buildable = exes.buildable in
    match buildable.ctypes with
    | None ->
      Exe.build_and_link_many
        cctx
        ~programs
        ~linkages
        ~link_args
        ~o_files
        ~promote:exes.promote
        ~embed_in_plugin_libraries
        ~sandbox
    | Some _ ->
      (* Ctypes stubgen builds utility .exe files that need to share modules
         with this compilation context. To support that, we extract the one-time
         run bits from [Exe.build_and_link_many] and run them here, then pass
         that to the [Exe.link_many] call here as well as the Ctypes_rules. This
         dance is done to avoid triggering duplicate rule exceptions. *)
      let+ () =
        let loc = fst (Nonempty_list.hd exes.names) in
        Ctypes_rules.gen_rules ~cctx ~buildable ~loc ~sctx ~scope ~dir
      and+ () = Module_compilation.build_all cctx
      and+ link =
        Exe.link_many
          ~programs
          ~linkages
          ~link_args
          ~o_files
          ~promote:exes.promote
          ~embed_in_plugin_libraries
          cctx
          ~sandbox
      in
      link
  in
  let+ () =
    Memo.parallel_iter dep_graphs.for_exes ~f:(Check_rules.add_cycle_check sctx ~dir)
  in
  ( cctx
  , Merlin.make
      ~requires:requires_link
      ~stdlib_dir
      ~flags
      ~modules
      ~source_dirs:Path.Source.Set.empty
      ~libname:None
      ~obj_dir
      ~preprocess:
        (Preprocess.Per_module.without_instrumentation exes.buildable.preprocess)
      ~dialects:(Dune_project.dialects (Scope.project scope))
      ~ident:(Lib.Compile.merlin_ident compile_info)
      ~modes:`Exe )
;;

let compile_info ~scope (exes : Executables.t) =
  let dune_version = Scope.project scope |> Dune_project.dune_version in
  let+ pps =
    (* TODO resolution should be delayed *)
    Resolve.Memo.read_memo
      (Preprocess.Per_module.with_instrumentation
         exes.buildable.preprocess
         ~instrumentation_backend:(Lib.DB.instrumentation_backend (Scope.libs scope)))
    >>| Preprocess.Per_module.pps
  in
  let names = Nonempty_list.to_list exes.names in
  let merlin_ident = Merlin_ident.for_exes ~names:(List.map ~f:snd names) in
  Lib.DB.resolve_user_written_deps
    (Scope.libs scope)
    (`Exe names)
    exes.buildable.libraries
    ~pps
    ~dune_version
    ~allow_overlaps:exes.buildable.allow_overlapping_dependencies
    ~forbidden_libraries:exes.forbidden_libraries
    ~merlin_ident
;;

let rules ~sctx ~dir ~dir_contents ~scope ~expander (exes : Executables.t) =
  let* compile_info = compile_info ~scope exes in
  let f () =
    executables_rules
      exes
      ~sctx
      ~dir
      ~dir_contents
      ~scope
      ~expander
      ~compile_info
      ~embed_in_plugin_libraries:exes.embed_in_plugin_libraries
  in
  let* () = Buildable_rules.gen_select_rules sctx compile_info ~dir
  and* () =
    let requires_link = Lib.Compile.requires_link compile_info in
    Bootstrap_info.gen_rules sctx exes ~dir ~requires_link
  in
  Buildable_rules.with_lib_deps (Super_context.context sctx) compile_info ~dir ~f
;;
