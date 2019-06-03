open Import
open! No_io

module Pp_spec : sig
  type t

  val make
    :  Dune_file.Preprocess.t Dune_file.Per_module.t
    -> Ocaml_version.t
    -> t

  val pped_modules : t -> Module.Name_map.t -> Module.Name_map.t
end = struct
  type t = (Module.t -> Module.t) Dune_file.Per_module.t

  let make preprocess v =
    Dune_file.Per_module.map preprocess ~f:(fun pp ->
      match Dune_file.Preprocess.remove_future_syntax ~for_:Compiler pp v with
      | No_preprocessing -> Module.ml_source
      | Action (_, _) ->
        fun m -> Module.ml_source (Module.pped m)
      | Pps { loc = _; pps = _; flags = _; staged } ->
        if staged then
          Module.ml_source
        else
          fun m -> Module.pped (Module.ml_source m))

  let pped_modules (t : t) modules =
    Module.Name.Map.map modules ~f:(fun (m : Module.t) ->
      Dune_file.Per_module.get t (Module.name m) m)
end

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
  let add_rule = Super_context.add_rule ~dir in
  let modes =
    Dune_file.Mode_conf.Set.eval impl.modes
      ~has_native:(Option.is_some ctx.ocamlopt) in
  let copy_obj_file ~src ~dst ?ext kind =
    let src = Module.cm_file_unsafe src ?ext kind in
    let dst = Module.cm_file_unsafe dst ?ext kind in
    copy_to_obj_dir ~src ~dst:(Path.as_in_build_dir_exn dst) in
  let copy_objs src =
    let dst = Module.set_obj_dir ~obj_dir:impl_obj_dir src in
    copy_obj_file ~src ~dst Cmi;
    if Module.is_public dst && Obj_dir.need_dedicated_public_dir impl_obj_dir
    then begin
      let src = Module.cm_public_file_unsafe src Cmi in
      let dst = Module.cm_public_file_unsafe dst Cmi in
      copy_to_obj_dir ~src ~dst:(Path.as_in_build_dir_exn dst)
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
      Path.Build.relative obj_dir (Path.basename f ^ ".all-deps") in
    if Lib.is_local vlib then
      fun m ->
        if Module.is_public m then
          List.iter [Intf; Impl] ~f:(fun kind ->
            Module.file m kind
            |> Option.iter ~f:(fun f ->
              copy_to_obj_dir
                ~src:(Path.build
                        (all_deps
                           ~obj_dir:
                           (Path.as_in_build_dir_exn
                              (Obj_dir.obj_dir vlib_obj_dir)) f))
                ~dst:(all_deps
                        ~obj_dir:(Path.as_in_build_dir_exn
                                    (Obj_dir.obj_dir impl_obj_dir)) f))
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
              (all_deps ~obj_dir:(Path.as_in_build_dir_exn
                                    (Obj_dir.obj_dir impl_obj_dir)) f)
            |> add_rule sctx))
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

let external_dep_graph sctx ~impl_cm_kind ~vlib_obj_dir ~impl_obj_dir
      ~vlib_modules =
  let wrapped = Lib_modules.is_wrapped vlib_modules in
  let modules = Lib_modules.modules vlib_modules in
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
    let deps_from_objinfo ~for_module (ocamlobjinfo : Ocamlobjinfo.t) =
      Module.Name.Set.to_list ocamlobjinfo.intf
      |> List.filter_map ~f:(fun dep ->
        match Module.Name.split_alias_prefix dep, wrapped with
        | Some _, false -> None
        | None, false ->
          if Module.name for_module = dep then
            None
          else
            Module.Name.Map.find modules dep
        | None, true -> (* lib interface module *)
          if Module.name for_module = dep then
            None
          else
            Lib_modules.main_module_name vlib_modules
            |> Option.bind ~f:(fun main_module_name ->
              if main_module_name = Module.name for_module then
                Module.Name.Map.find modules dep
              else
                None)
        | Some (prefix, name), true ->
          begin match Lib_modules.main_module_name vlib_modules with
          | None -> assert false
          | Some main_module_name ->
            if main_module_name <> prefix
            || Module.name for_module = name then
              None
            else
              Module.Name.Map.find modules name
          end)
    in
    Dep_graph.make ~dir:impl_obj_dir
      ~per_module:(Module.Name.Map.map modules ~f:(fun m ->
        let deps =
          if (ml_kind = Intf && not (Module.has_intf m))
          || (ml_kind = Impl && not (Module.has_impl m))
          then
            Build.return []
          else
            let (write, read) = ocamlobjinfo m cm_kind in
            Super_context.add_rule sctx ~dir:impl_obj_dir write;
            let open Build.O in
            Build.memoize "ocamlobjinfo" @@
            read >>^ deps_from_objinfo ~for_module:m
        in
        m, deps)))

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
      let (vlib_modules, vlib_foreign_objects) =
        match virtual_, Lib.foreign_objects vlib with
        | External _, Local
        | Local, External _ -> assert false
        | External lib_modules, External fa -> (lib_modules, fa)
        | Local, Local ->
          let name = Lib.name vlib in
          let dir_contents =
            Dir_contents.get_without_rules sctx ~dir:(
              Path.as_in_build_dir_exn (Lib.src_dir vlib)) in
          let modules =
            let pp_spec =
              Pp_spec.make lib.buildable.preprocess
                (Super_context.context sctx).version
            in
            let modules = Dir_contents.modules_of_library dir_contents ~name in
            Lib_modules.modules modules
            |> Pp_spec.pped_modules pp_spec
            |> Lib_modules.set_modules modules
          in
          let foreign_objects =
            let ext_obj = (Super_context.context sctx).ext_obj in
            let dir = Obj_dir.obj_dir (Lib.obj_dir vlib) in
            Dir_contents.c_sources_of_library dir_contents ~name
            |> C.Sources.objects ~ext_obj ~dir:(Path.as_in_build_dir_exn dir)
            |> List.map ~f:Path.build
          in
          (modules, foreign_objects)
      in
      let virtual_modules = Lib_modules.virtual_modules vlib_modules in
      check_module_fields ~lib ~virtual_modules ~modules ~implements;
      let vlib_dep_graph =
        let vlib_obj_dir = Lib.obj_dir vlib in
        let modules = Lib_modules.modules vlib_modules in
        match virtual_ with
        | Local ->
          let obj_dir =
            Path.as_in_build_dir_exn (Obj_dir.obj_dir vlib_obj_dir) in
          Ocamldep.graph_of_remote_lib ~obj_dir ~modules
        | External _ ->
          let impl_obj_dir =
            Utils.library_object_directory ~dir (snd lib.name) in
          let impl_cm_kind =
            let { Mode.Dict. byte; native = _ } = Lib.modes vlib in
            Mode.cm_kind (if byte then Byte else Native)
          in
          external_dep_graph sctx ~impl_cm_kind ~vlib_obj_dir ~impl_obj_dir
            ~vlib_modules
      in
      Vimpl.make ~dir
        ~impl:lib ~vlib ~vlib_modules ~vlib_dep_graph
        ~vlib_foreign_objects
  end
