open Import
open! No_io

let setup_copy_rules_for_impl ~sctx ~dir vimpl =
  let ctx = Super_context.context sctx in
  let vlib = Vimpl.vlib vimpl in
  let impl = Vimpl.impl vimpl in
  let impl_obj_dir = Dune_file.Library.obj_dir ~dir impl in
  let vlib_obj_dir = Lib.obj_dir vlib in
  let vlib_modules = Vimpl.vlib_modules vimpl in
  let copy_to_obj_dir ~src ~dst =
    Super_context.add_rule ~dir ~loc:(Loc.of_pos __POS__)
      sctx (Build.symlink ~src ~dst)
  in
  let modes =
    Dune_file.Mode_conf.Set.eval impl.modes
      ~has_native:(Option.is_some ctx.ocamlopt) in
  let copy_obj_file ~src ~dst ?ext kind =
    let src = Module.cm_file_unsafe src ?ext kind in
    let dst = Module.cm_file_unsafe dst ?ext kind in
    copy_to_obj_dir ~src ~dst in
  let copy_objs src =
    let dst = Module.set_obj_dir ~obj_dir:impl_obj_dir src in
    copy_obj_file ~src ~dst Cmi;
    if Module.is_public dst && Obj_dir.has_public_cmi_dir impl_obj_dir
    then begin
      let src = Module.cm_public_file_unsafe src Cmi in
      let dst = Module.cm_public_file_unsafe dst Cmi in
      copy_to_obj_dir ~src ~dst
    end;
    if Module.has_impl src then begin
      if modes.byte then
        copy_obj_file ~src ~dst Cmo;
      if modes.native then
        List.iter [Cm_kind.ext Cmx; ctx.ext_obj]
          ~f:(fun ext -> copy_obj_file ~src ~dst ~ext Cmx)
    end
  in
  let copy_all_deps =
    let all_deps ~obj_dir f =
      Path.relative obj_dir (Path.basename f ^ ".all-deps") in
    if Lib.is_local vlib then
      fun m ->
        if Module.is_public m then
          List.iter [Intf; Impl] ~f:(fun kind ->
            Module.file m kind
            |> Option.iter ~f:(fun f ->
              copy_to_obj_dir
                ~src:(all_deps ~obj_dir:(Obj_dir.obj_dir vlib_obj_dir) f)
                ~dst:(all_deps ~obj_dir:(Obj_dir.obj_dir impl_obj_dir) f))
          );
    else
      (* we only need to copy the .all-deps files for local libraries. for
         remote libraries, we just use ocamlobjinfo *)
      let vlib_dep_graph = Vimpl.vlib_dep_graph vimpl in
      fun m ->
        List.iter [Intf; Impl] ~f:(fun kind ->
          let dep_graph = Ml_kind.Dict.get vlib_dep_graph kind in
          let deps = Dep_graph.deps_of dep_graph m in
          Module.file m kind |> Option.iter ~f:(fun f ->
            let open Build.O in
            deps >>^ (fun modules ->
              modules
              |> List.map ~f:(fun m -> Module.Name.to_string (Module.name m))
              |> String.concat ~sep:"\n")
            >>>
            Build.write_file_dyn
              (all_deps ~obj_dir:(Obj_dir.obj_dir impl_obj_dir) f)
            |> Super_context.add_rule sctx ~dir))
  in
  Option.iter (Lib_modules.alias_module vlib_modules) ~f:copy_objs;
  Module.Name.Map.iter (Lib_modules.modules vlib_modules)
    ~f:(fun m -> copy_objs m; copy_all_deps m)


let module_list ms =
  List.map ms ~f:(fun m -> sprintf "- %s" (Module.Name.to_string m))
  |> String.concat ~sep:"\n"

let check_module_fields ~(lib : Dune_file.Library.t) ~virtual_modules
      ~modules ~implements =
  let new_public_modules =
    Module.Name.Map.foldi modules ~init:[] ~f:(fun name m acc ->
      if Module.is_public m
      && not (Module.Name.Map.mem virtual_modules name) then
        name :: acc
      else
        acc)
  in
  if new_public_modules <> [] then begin
    Errors.fail lib.buildable.loc
      "The following modules aren't part of the virtual library's interface:\
       \n%s\n\
       They must be marked as private using the (private_modules ..) field"
      (module_list new_public_modules)
  end;
  let (missing_modules, impl_modules_with_intf, private_virtual_modules) =
    Module.Name.Map.foldi virtual_modules ~init:([], [], [])
      ~f:(fun m _ (mms, ims, pvms) ->
        match Module.Name.Map.find modules m with
        | None -> (m :: mms, ims, pvms)
        | Some m ->
          let ims =
            if Module.has_intf m then
              Module.name m :: ims
            else
              ims
          in
          let pvms =
            if Module.is_public m then
              pvms
            else
              Module.name m :: pvms
          in
          (mms, ims, pvms))
  in
  if private_virtual_modules <> [] then begin
    (* The loc here will never be none as we've some private modules *)
    Errors.fail_opt (Option.bind lib.private_modules ~f:Ordered_set_lang.loc)
      "These private modules cannot be private:\n%s"
      (module_list private_virtual_modules)
  end;
  if missing_modules <> [] then begin
    Errors.fail lib.buildable.loc
      "Library %a cannot implement %a because the following \
       modules lack an implementation:\n%s"
      Lib_name.Local.pp (snd lib.name)
      Lib_name.pp implements
      (module_list missing_modules)
  end;
  if impl_modules_with_intf <> [] then begin
    Errors.fail lib.buildable.loc
      "The following modules cannot have .mli files as they implement \
       virtual modules:\n%s"
      (module_list impl_modules_with_intf)
  end

let external_dep_graph sctx ~impl_cm_kind ~vlib_obj_dir ~impl_obj_dir ~modules =
  let ocamlobjinfo =
    let ctx = Super_context.context sctx in
    fun m cm_kind ->
      let m = Module.set_obj_dir ~obj_dir:vlib_obj_dir m in
      let unit = Module.cm_file_unsafe m cm_kind in
      Ocamlobjinfo.rules ~dir:impl_obj_dir ~ctx ~unit
  in
  Ml_kind.Dict.of_func (fun ~ml_kind ->
    let cm_kind =
      match ml_kind with
      | Impl -> impl_cm_kind
      | Intf -> Cm_kind.Cmi
    in
    Dep_graph.make ~dir:impl_obj_dir
      ~per_module:(Module.Name.Map.map modules ~f:(fun m ->
        if (ml_kind = Intf && not (Module.has_intf m))
        || (ml_kind = Impl && not (Module.has_impl m))
        then
          (m, Build.return [])
        else
          let (write, read) = ocamlobjinfo m cm_kind in
          Super_context.add_rule sctx ~dir:impl_obj_dir write;
          let open Build.O in
          ( m
          , Build.memoize "ocamlobjinfo" @@
            read >>^ fun dict ->
            Module.Name.Set.to_list dict.intf
            |> List.filter_map ~f:(fun dep ->
              let dep = Module.Name.strip_alias_prefix dep in
              if Module.name m = dep then
                None
              else
                Module.Name.Map.find modules dep)))))

let impl sctx ~dir ~(lib : Dune_file.Library.t) ~scope ~modules =
  Option.map lib.implements ~f:begin fun (loc, implements) ->
    match Lib.DB.find (Scope.libs scope) implements with
    | Error _ ->
      Errors.fail loc
        "Cannot implement %a as that library isn't available"
        Lib_name.pp implements
    | Ok vlib ->
      let virtual_ =
        match Lib.virtual_ vlib with
        | None ->
          Errors.fail lib.buildable.loc
            "Library %a isn't virtual and cannot be implemented"
            Lib_name.pp implements
        | Some v -> v
      in
      let vlib_modules =
        match virtual_ with
        | External lib_modules -> lib_modules
        | Local ->
          let dir_contents =
            Dir_contents.get sctx ~dir:(Lib.src_dir vlib) in
          Dir_contents.modules_of_library dir_contents
            ~name:(Lib.name vlib)
      in
      let virtual_modules = Lib_modules.virtual_modules vlib_modules in
      check_module_fields ~lib ~virtual_modules ~modules ~implements;
      let vlib_dep_graph =
        let vlib_obj_dir = Lib.obj_dir vlib in
        let modules = Lib_modules.modules vlib_modules in
        match virtual_ with
        | Local ->
          Ocamldep.graph_of_remote_lib
            ~obj_dir:(Obj_dir.obj_dir vlib_obj_dir) ~modules
        | External _ ->
          let impl_obj_dir =
            Utils.library_object_directory ~dir (snd lib.name) in
          let impl_cm_kind =
            let { Mode.Dict. byte; native = _ } = Lib.modes vlib in
            if byte then
              Mode.cm_kind Byte
            else
              Mode.cm_kind Native
          in
          external_dep_graph sctx ~impl_cm_kind ~vlib_obj_dir ~impl_obj_dir
            ~modules
      in
      Vimpl.make ~dir ~impl:lib ~vlib ~vlib_modules ~vlib_dep_graph
  end
