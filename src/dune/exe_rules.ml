open Import
open! No_io
open Build.O
module SC = Super_context
module Executables = Dune_file.Executables

let executables_rules ~sctx ~dir ~expander ~dir_contents ~scope ~compile_info
    (exes : Dune_file.Executables.t) =
  (* Use "eobjs" rather than "objs" to avoid a potential conflict with a
     library of the same name *)
  let obj_dir = Dune_file.Executables.obj_dir exes ~dir in
  Check_rules.add_obj_dir sctx ~obj_dir;
  let first_exe = snd (List.hd exes.names) in
  let modules =
    Dir_contents.modules_of_executables dir_contents ~first_exe ~obj_dir
  in
  let pp =
    Preprocessing.make sctx ~dir ~dep_kind:Required ~scope ~expander
      ~preprocess:exes.buildable.preprocess
      ~preprocessor_deps:exes.buildable.preprocessor_deps
      ~lint:exes.buildable.lint ~lib_name:None
  in
  let modules =
    Modules.map_user_written modules ~f:(fun m ->
        let name = Module.name m in
        Preprocessing.pp_module_as pp name m)
  in
  let programs =
    List.map exes.names ~f:(fun (loc, name) ->
        let mod_name = Module_name.of_string name in
        match Modules.find modules mod_name with
        | Some m ->
          if not (Module.has m ~ml_kind:Impl) then
            User_error.raise ~loc
              [ Pp.textf "Module %S has no implementation."
                  (Module_name.to_string mod_name)
              ]
          else
            { Exe.Program.name; main_module_name = mod_name; loc }
        | None ->
          User_error.raise ~loc
            [ Pp.textf "Module %S doesn't exist."
                (Module_name.to_string mod_name)
            ])
  in
  let explicit_js_mode = Dune_project.explicit_js_mode (Scope.project scope) in
  let linkages =
    let module L = Dune_file.Executables.Link_mode in
    let ctx = SC.context sctx in
    let l =
      let has_native = Option.is_some ctx.ocamlopt in
      let modes =
        let f = function
          | { L.kind = Js; _ } -> true
          | _ -> false
        in
        if L.Set.exists exes.modes ~f then
          L.Set.add exes.modes L.byte_exe
        else if (not explicit_js_mode) && L.Set.mem exes.modes L.byte_exe then
          L.Set.add exes.modes L.js
        else
          exes.modes
      in
      List.filter_map (L.Set.to_list modes) ~f:(fun (mode : L.t) ->
          match (has_native, mode.mode) with
          | false, Native -> None
          | _ -> Some (Exe.Linkage.of_user_config ctx mode))
    in
    (* If bytecode was requested but not native or best version, add custom
       linking *)
    if
      L.Set.mem exes.modes L.byte
      && (not (L.Set.mem exes.modes L.native))
      && not (L.Set.mem exes.modes L.exe)
    then
      Exe.Linkage.custom ctx :: l
    else
      l
  in
  let flags = SC.ocaml_flags sctx ~dir exes.buildable in
  let link_deps = SC.Deps.interpret sctx ~expander exes.link_deps in
  let archive_names = exes.buildable.foreign_archives |> List.map ~f:snd in
  let link_flags =
    link_deps
    >>> Expander.expand_and_eval_set expander exes.link_flags
          ~standard:(Build.return [])
  in
  let link_flags =
    let+ flags = link_flags in
    flags
    @ List.concat_map archive_names ~f:(fun archive_name ->
          [ "-cclib"; "-l" ^ archive_name ])
  in
  let requires_compile = Lib.Compile.direct_requires compile_info in
  let cctx =
    let requires_link = Lib.Compile.requires_link compile_info in
    let js_of_ocaml =
      let js_of_ocaml = exes.buildable.js_of_ocaml in
      if explicit_js_mode then
        Option.some_if (List.mem ~set:linkages Exe.Linkage.js) js_of_ocaml
      else
        Some js_of_ocaml
    in
    let dynlink =
      (* See https://github.com/ocaml/dune/issues/2527 *)
      true
      || Dune_file.Executables.Link_mode.Set.exists exes.modes ~f:(fun mode ->
             match mode.kind with
             | Shared_object -> true
             | _ -> false)
    in
    Compilation_context.create () ~super_context:sctx ~expander ~scope ~obj_dir
      ~modules ~flags ~requires_link ~requires_compile ~preprocessing:pp
      ~js_of_ocaml ~opaque:(SC.opaque sctx) ~dynlink ~package:exes.package
  in
  let o_files =
    if not (Executables.has_stubs exes) then
      []
    else
      let what =
        if List.is_empty exes.buildable.Dune_file.Buildable.foreign_stubs then
          "archives"
        else
          "stubs"
      in
      if List.mem Exe.Linkage.byte ~set:linkages then
        User_error.raise ~loc:exes.buildable.loc
          [ Pp.textf "Pure bytecode executables cannot contain foreign %s."
              what
          ]
          ~hints:
            [ Pp.text
                "If you need to build only a native executable use \"(modes \
                 exe)\"."
            ];
      let foreign_sources =
        Dir_contents.foreign_sources_of_executables dir_contents ~first_exe
      in
      let o_files =
        Foreign_rules.build_o_files ~sctx ~dir ~expander
          ~requires:requires_compile ~dir_contents ~foreign_sources
          ~extra_flags:Command.Args.empty ~extra_deps:[]
        |> List.map ~f:Path.build
      in
      Check_rules.add_files sctx ~dir o_files;
      o_files
  in
  let requires_compile = Compilation_context.requires_compile cctx in
  Exe.build_and_link_many cctx ~programs ~linkages ~link_flags ~o_files
    ~promote:exes.promote;
  ( cctx
  , Merlin.make () ~requires:requires_compile ~flags ~modules
      ~preprocess:(Dune_file.Buildable.single_preprocess exes.buildable)
      ~obj_dir )

let rules ~sctx ~dir ~dir_contents ~scope ~expander
    (exes : Dune_file.Executables.t) =
  let compile_info =
    Lib.DB.resolve_user_written_deps_for_exes (Scope.libs scope) exes.names
      exes.buildable.libraries
      ~pps:(Dune_file.Preprocess_map.pps exes.buildable.preprocess)
      ~allow_overlaps:exes.buildable.allow_overlapping_dependencies
      ~variants:exes.variants ~optional:exes.optional
      ~forbidden_libraries:exes.forbidden_libraries
  in
  let f () =
    executables_rules exes ~sctx ~dir ~dir_contents ~scope ~expander
      ~compile_info
  in
  SC.Libs.gen_select_rules sctx compile_info ~dir;
  Bootstrap_info.gen_rules sctx exes ~dir compile_info;
  SC.Libs.with_lib_deps sctx compile_info ~dir ~f
