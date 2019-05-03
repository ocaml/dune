open! Stdune
open Import
open Build.O

module CC = Compilation_context
module SC = Super_context

let is_alias_module cctx (m : Module.t) =
  let open Module.Name.Infix in
  match CC.alias_module cctx with
  | None -> false
  | Some alias -> Module.name alias = Module.name m

let parse_module_names ~(unit : Module.t) ~modules words =
  let open Module.Name.Infix in
  List.filter_map words ~f:(fun m ->
    let m = Module.Name.of_string m in
    if m = Module.name unit then
      None
    else
      Module.Name.Map.find modules m)

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
  let vimpl                = CC.vimpl                cctx in
  let deps =
    let modules = Vimpl.add_vlib_modules vimpl modules in
    parse_module_names ~unit ~modules deps
  in
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
      let open Module.Name.Infix in
      if Module.name unit <> m
      && not (is_alias_module cctx unit)
      && List.exists deps ~f:(fun x -> Module.name x = m) then
        die "Module %a in directory %s depends on %a.\n\
             This doesn't make sense to me.\n\
             \n\
             %a is the main module of the library and is \
             the only module exposed \n\
             outside of the library. Consequently, it should \
             be the one depending \n\
             on all the other modules in the library."
          Module.Name.pp (Module.name unit) (Path.to_string dir)
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
      match CC.lib_interface_module cctx with
      | None -> deps
      | Some m ->
        if Module.name unit = Module.name m then
          deps
        else
          m :: deps

let deps_of cctx ~ml_kind unit =
  let sctx = CC.super_context cctx in
  if is_alias_module cctx unit then
    Build.return []
  else
    match Module.file unit ml_kind with
    | None -> Build.return []
    | Some file ->
      let dir = Compilation_context.dir cctx in
      let file_in_obj_dir ~suffix file =
        let base = Path.basename file in
        Path.relative_exn
          (Obj_dir.obj_dir (Compilation_context.obj_dir cctx))
          (base ^ suffix)
      in
      let all_deps_path file = file_in_obj_dir file ~suffix:".all-deps" in
      let context = SC.context sctx in
      let vimpl = Compilation_context.vimpl cctx in
      let parse_module_names =
        let modules = Vimpl.add_vlib_modules vimpl (CC.modules cctx) in
        parse_module_names ~modules
      in
      let all_deps_file = all_deps_path file in
      let ocamldep_output = file_in_obj_dir file ~suffix:".d" in
      SC.add_rule sctx ~dir
        (let flags =
           Option.value (Module.pp_flags unit) ~default:(Build.return []) in
         flags >>>
         Build.run (Ok context.ocamldep) ~dir:context.build_dir
           [ A "-modules"
           ; Dyn (fun flags -> As flags)
           ; Ml_kind.flag ml_kind
           ; Dep file
           ]
           ~stdout_to:ocamldep_output
        );
      let build_paths dependencies =
        let dependency_file_path m =
          let file_path m =
            if is_alias_module cctx m then
              None
            else
              match Module.file m Ml_kind.Intf with
              | Some _ as x -> x
              | None ->
                Module.file m Ml_kind.Impl
          in
          let module_file_ =
            match file_path m with
            | Some v -> Some v
            | None -> Option.bind ~f:file_path (Vimpl.find_module vimpl m)
          in
          Option.map ~f:all_deps_path module_file_
        in
        List.filter_map dependencies ~f:dependency_file_path
      in
      SC.add_rule sctx ~dir
        ( Build.lines_of ocamldep_output
          >>^ parse_deps_exn ~file
          >>^ interpret_deps cctx ~unit
          >>^ (fun modules ->
            (build_paths modules,
             List.map modules ~f:(fun m ->
               Module.Name.to_string (Module.name m))
            ))
          >>> Build.merge_files_dyn ~target:all_deps_file);
      Build.memoize (Path.to_string all_deps_file)
        ( Build.lines_of all_deps_file
          >>^ parse_module_names ~unit)

let rules_generic cctx ~modules =
  Ml_kind.Dict.of_func
    (fun ~ml_kind ->
       let per_module =
         Module.Name.Map.map modules ~f:(fun m -> (m, deps_of cctx ~ml_kind m))
       in
       Dep_graph.make ~dir:(CC.dir cctx) ~per_module)

let rules cctx = rules_generic cctx ~modules:(CC.modules cctx)

let rules_for_auxiliary_module cctx (m : Module.t) =
  rules_generic cctx ~modules:(Module.Name.Map.singleton (Module.name m) m)

let graph_of_remote_lib ~obj_dir ~modules =
  let deps_of unit ~ml_kind =
    match Module.file unit ml_kind with
    | None -> Build.return []
    | Some file ->
      let file_in_obj_dir ~suffix file =
        let base = Path.basename file in
        Path.relative_exn obj_dir (base ^ suffix)
      in
      let all_deps_path file = file_in_obj_dir file ~suffix:".all-deps" in
      let all_deps_file = all_deps_path file in
      Build.memoize (Path.to_string all_deps_file)
        (Build.lines_of all_deps_file
         >>^ parse_module_names ~unit ~modules)
  in
  Ml_kind.Dict.of_func (fun ~ml_kind ->
    let per_module =
      Module.Name.Map.map modules ~f:(fun m -> (m, deps_of ~ml_kind m)) in
    Dep_graph.make ~dir:obj_dir ~per_module)
