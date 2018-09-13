open Import
open! No_io

module Implementation = struct
  type t =
    { vlib            : Lib.t
    ; impl            : Dune_file.Library.t
    ; vlib_modules    : Module.t Module.Name.Map.t
    }
end

module Gen (P : sig val sctx : Super_context.t end) = struct
  open P
  let ctx = Super_context.context sctx

  let vlib_stubs_o_files { Implementation.vlib ; _ } =
    Lib.foreign_objects vlib ~ext:ctx.ext_obj

  let setup_copy_rules_for_impl ~dir
        { Implementation.vlib ; impl ; vlib_modules } =
    let copy_to_obj_dir =
      let obj_dir = Utils.library_object_directory ~dir (snd impl.name) in
      fun file ->
        let dst = Path.relative obj_dir (Path.basename file) in
        Super_context.add_rule sctx (Build.symlink ~src:file ~dst)
    in
    let obj_dir = Lib.obj_dir vlib in
    let modes =
      Dune_file.Mode_conf.Set.eval impl.modes
        ~has_native:(Option.is_some ctx.ocamlopt) in
    Module.Name.Map.iter vlib_modules ~f:(fun m ->
      let copy_obj_file ext =
        copy_to_obj_dir (Module.obj_file m ~obj_dir ~ext) in
      copy_obj_file (Cm_kind.ext Cmi);
      if Module.has_impl m then begin
        if modes.byte then
          copy_obj_file (Cm_kind.ext Cmo);
        if modes.native then
          List.iter [Cm_kind.ext Cmx; ctx.ext_obj] ~f:copy_obj_file
      end)

  let check_virtual_modules_field ~(lib : Dune_file.Library.t) ~virtual_modules
        ~modules ~implements =
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
    let module_list ms =
      List.map ms ~f:Module.Name.to_string
      |> String.concat ~sep:"\n"
    in
    if private_virtual_modules <> [] then begin
      (* The loc here will never be none as we've some private modules *)
      Errors.fail_opt (Ordered_set_lang.loc lib.private_modules)
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

  let impl ~(lib : Dune_file.Library.t) ~scope ~modules =
    Option.map lib.implements ~f:begin fun (loc, implements) ->
      match Lib.DB.find (Scope.libs scope) implements with
      | Error _ ->
        Errors.fail loc
          "Cannot implement %a as that library isn't available"
          Lib_name.pp implements
      | Ok vlib ->
        let virtual_modules =
          Option.map (Lib.virtual_ vlib) ~f:(fun (v : Lib_info.Virtual.t) ->
            v.modules)
        in
        let (vlib_modules, virtual_modules) =
          match virtual_modules with
          | None ->
            Errors.fail lib.buildable.loc
              "Library %a isn't virtual and cannot be implemented"
              Lib_name.pp implements
          | Some Unexpanded ->
            let dir_contents =
              Dir_contents.get sctx ~dir:(Lib.src_dir vlib) in
            let lib_modules =
              Dir_contents.modules_of_library dir_contents
                ~name:(Lib.name vlib) in
            ( Lib_modules.modules lib_modules
            , Lib_modules.virtual_modules lib_modules
            )
        in
        check_virtual_modules_field ~lib ~virtual_modules ~modules ~implements;
        { Implementation.
          impl = lib
        ; vlib
        ; vlib_modules
        }
    end
end
