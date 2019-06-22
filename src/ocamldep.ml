open! Stdune
open Import
open Build.O

module CC = Compilation_context
module SC = Super_context

let parse_module_names ~(unit : Module.t) ~modules words =
  let open Module.Name.Infix in
  List.filter_map words ~f:(fun m ->
    let m = Module.Name.of_string m in
    if m = Module.name unit then
      None
    else
      Modules.find_dep modules m)

let parse_deps_exn ~file lines =
  let invalid () =
    die "ocamldep returned unexpected output for %s:\n\
         %s"
      (Path.to_string_maybe_quoted file)
      (String.concat ~sep:"\n"
         (List.map lines ~f:(sprintf "> %s")))
  in
  match lines with
  | [] | _ :: _ :: _ -> invalid ()
  | [line] ->
    match String.lsplit2 line ~on:':' with
    | None -> invalid ()
    | Some (basename, deps) ->
      let basename = Filename.basename basename in
      if basename <> Path.basename file then invalid ();
      String.extract_blank_separated_words deps

let interpret_deps cctx ~unit deps =
  let dir                  = CC.dir                  cctx in
  let alias_module         = CC.alias_module         cctx in
  let lib_interface_module = CC.lib_interface_module cctx in
  let modules              = CC.modules              cctx in
  let deps = parse_module_names ~unit ~modules deps in
  let stdlib = CC.stdlib cctx in
  let deps =
    match stdlib, CC.lib_interface_module cctx with
    | Some { modules_before_stdlib; _ }, Some m
      when Module.name unit = Module.name m ->
      (* See comment in [Dune_file.Stdlib]. *)
      List.filter deps ~f:(fun m ->
        Module.Name.Set.mem modules_before_stdlib (Module.name m))
    | _ -> deps
  in
  if Option.is_none stdlib then
    Option.iter lib_interface_module ~f:(fun (m : Module.t) ->
      let m = Module.name m in
      if Module.Name.Infix.(Module.name unit <> m)
      && not (Module.kind unit = Alias)
      && List.exists deps ~f:(fun x -> Module.name x = m) then
        die "Module %a in directory %s depends on %a.\n\
             This doesn't make sense to me.\n\
             \n\
             %a is the main module of the library and is \
             the only module exposed \n\
             outside of the library. Consequently, it should \
             be the one depending \n\
             on all the other modules in the library."
          Module.Name.pp (Module.name unit) (Path.to_string (Path.build dir))
          Module.Name.pp m
          Module.Name.pp m);
  match stdlib with
  | None -> begin
      match alias_module with
      | None -> deps
      | Some m -> m :: deps
    end
  | Some { modules_before_stdlib; _ } ->
    if Module.Name.Set.mem modules_before_stdlib (Module.name unit) then
      deps
    else
      match lib_interface_module with
      | None -> deps
      | Some m ->
        if Module.name unit = Module.name m then
          deps
        else
          m :: deps

let deps_of cctx ~ml_kind unit =
  let kind = Module.kind unit in
  let modules = Compilation_context.modules cctx in
  match kind with
  | Alias -> Build.return []
  | Wrapped_compat ->
    begin match Modules.lib_interface modules with
    | Some m -> m
    | None -> Modules.compat_for_exn modules unit
    end
    |> List.singleton
    |> Build.return
  | _ ->
    let sctx = CC.super_context cctx in
    begin match Module.source unit ~ml_kind with
    | None -> Build.return []
    | Some source ->
      let obj_dir = Compilation_context.obj_dir cctx in
      let dir = Compilation_context.dir cctx in
      let dep = Obj_dir.Module.dep obj_dir in
      let context = SC.context sctx in
      let parse_module_names = parse_module_names ~modules in
      let all_deps_file = dep source ~kind:Transitive in
      let ocamldep_output = dep source ~kind:Immediate in
      SC.add_rule sctx ~dir
        (let flags =
           Option.value (Module.pp_flags unit) ~default:(Build.return []) in
         Command.run (Ok context.ocamldep) ~dir:(Path.build context.build_dir)
           [ A "-modules"
           ; Command.Args.dyn flags
           ; Ml_kind.flag ml_kind
           ; Dep (Module.File.path source)
           ]
           ~stdout_to:ocamldep_output
        );
      let build_paths dependencies =
        let dependency_file_path m =
          let source m =
            if Module.kind m = Alias then
              None
            else
              match Module.source m ~ml_kind:Intf with
              | Some _ as x -> x
              | None -> Module.source m ~ml_kind:Impl
          in
          let module_file_ = source m in
          Option.map ~f:(fun p -> Path.build (dep p ~kind:Transitive)) module_file_
        in
        List.filter_map dependencies ~f:dependency_file_path
      in
      SC.add_rule sctx ~dir
        ( Build.lines_of (Path.build ocamldep_output)
          >>^ parse_deps_exn ~file:(Module.File.path source)
          >>^ interpret_deps cctx ~unit
          >>^ (fun modules ->
            (build_paths modules,
             List.map modules ~f:(fun m ->
               Module.Name.to_string (Module.name m))
            ))
          >>> Build.merge_files_dyn ~target:all_deps_file);
      let all_deps_file = Path.build all_deps_file in
      Build.memoize (Path.to_string all_deps_file)
        (Build.lines_of all_deps_file >>^ parse_module_names ~unit)
    end

let rules cctx ~modules =
  let dir = CC.dir cctx in
  Ml_kind.Dict.of_func (fun ~ml_kind ->
    let per_module =
      Modules.fold modules ~init:Module.Obj_map.empty ~f:(fun m acc ->
        Module.Obj_map.add acc m (deps_of cctx ~ml_kind m))
    in
    Dep_graph.make ~dir ~per_module)

let graph_of_remote_lib ~obj_dir ~modules =
  let deps_of unit ~ml_kind =
    match Module.source unit ~ml_kind with
    | None -> Build.return []
    | Some source ->
      let all_deps_file =
        Obj_dir.Module.dep obj_dir source ~kind:Transitive in
      Build.memoize (Path.Build.to_string all_deps_file)
        (Build.lines_of (Path.build all_deps_file)
         >>^ parse_module_names ~unit ~modules)
  in
  let dir = Obj_dir.dir obj_dir in
  Ml_kind.Dict.of_func (fun ~ml_kind ->
    let per_module =
      Modules.fold modules ~init:Module.Obj_map.empty ~f:(fun m acc ->
        Module.Obj_map.add acc m (deps_of ~ml_kind m))
    in
    Dep_graph.make ~dir ~per_module)
