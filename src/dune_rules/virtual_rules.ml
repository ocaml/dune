open Import

let setup_copy_rules_for_impl ~sctx ~dir vimpl =
  let ctx = Super_context.context sctx in
  let vlib = Vimpl.vlib vimpl in
  let impl = Vimpl.impl vimpl in
  let impl_obj_dir = Library.obj_dir ~dir impl in
  let vlib_obj_dir = Lib.info vlib |> Lib_info.obj_dir in
  let add_rule = Super_context.add_rule sctx ~dir in
  let copy_to_obj_dir ~src ~dst =
    add_rule ~loc:(Loc.of_pos __POS__) (Action_builder.symlink ~src ~dst)
  in
  let open Memo.O in
  let* { Lib_config.has_native; ext_obj; _ } =
    let+ ocaml = Context.ocaml ctx in
    ocaml.lib_config
  in
  let { Lib_mode.Map.ocaml = { byte; native }; melange } =
    Mode_conf.Lib.Set.eval impl.modes ~has_native
  in
  let copy_obj_file m kind =
    let src = Obj_dir.Module.cm_file_exn vlib_obj_dir m ~kind in
    let dst = Obj_dir.Module.cm_file_exn impl_obj_dir m ~kind in
    copy_to_obj_dir ~src ~dst
  in
  let copy_ocamldep_file m =
    match Obj_dir.to_local vlib_obj_dir with
    | None -> Memo.return ()
    | Some vlib_obj_dir ->
      let src = Obj_dir.Module.dep vlib_obj_dir (Immediate (m, Impl)) |> Path.build in
      let dst = Obj_dir.Module.dep impl_obj_dir (Immediate (m, Impl)) in
      copy_to_obj_dir ~src ~dst
  in
  let copy_interface_to_impl ~src kind () =
    let dst = Obj_dir.Module.cm_public_file_exn impl_obj_dir src ~kind in
    let src = Obj_dir.Module.cm_public_file_exn vlib_obj_dir src ~kind in
    copy_to_obj_dir ~src ~dst
  in
  let copy_objs src =
    Memo.when_ (byte || native) (fun () -> copy_obj_file src (Ocaml Cmi))
    >>> Memo.when_ melange (fun () -> copy_obj_file src (Melange Cmi))
    >>> Memo.when_
          (Module.visibility src = Public
           && Obj_dir.need_dedicated_public_dir impl_obj_dir)
          (fun () ->
            Memo.when_ (byte || native) (copy_interface_to_impl ~src (Ocaml Cmi))
            >>> Memo.when_ melange (copy_interface_to_impl ~src (Melange Cmi)))
    >>> Memo.when_ (Module.has src ~ml_kind:Impl) (fun () ->
      Memo.when_ byte (fun () -> copy_obj_file src (Ocaml Cmo))
      >>> Memo.when_ melange (fun () ->
        copy_obj_file src (Melange Cmj) >>> copy_ocamldep_file src)
      >>> Memo.when_ native (fun () ->
        copy_obj_file src (Ocaml Cmx)
        >>>
        let object_file dir = Obj_dir.Module.o_file_exn dir src ~ext_obj in
        copy_to_obj_dir ~src:(object_file vlib_obj_dir) ~dst:(object_file impl_obj_dir)))
  in
  let vlib_modules = Vimpl.vlib_modules vimpl in
  Modules.fold vlib_modules ~init:(Memo.return ()) ~f:(fun m acc -> acc >>> copy_objs m)
;;

let impl sctx ~(lib : Library.t) ~scope =
  let open Memo.O in
  match lib.implements with
  | None -> Memo.return None
  | Some (loc, implements) ->
    Lib.DB.find (Scope.libs scope) implements
    >>= (function
     | None ->
       User_error.raise
         ~loc
         [ Pp.textf
             "Cannot implement %s as that library isn't available"
             (Lib_name.to_string implements)
         ]
     | Some vlib ->
       let info = Lib.info vlib in
       let virtual_ =
         let virtual_ = Lib_info.virtual_ info in
         match virtual_ with
         | Some v -> v
         | None ->
           User_error.raise
             ~loc:lib.buildable.loc
             [ Pp.textf
                 "Library %s isn't virtual and cannot be implemented"
                 (Lib_name.to_string implements)
             ]
       in
       let+ vlib_modules, vlib_foreign_objects =
         let foreign_objects = Lib_info.foreign_objects info in
         match virtual_, foreign_objects with
         | External _, Local | Local, External _ -> assert false
         | External modules, External fa -> Memo.return (modules, fa)
         | Local, Local ->
           let name = Lib.name vlib in
           let vlib = Lib.Local.of_lib_exn vlib in
           let* dir_contents =
             let info = Lib.Local.info vlib in
             let dir = Lib_info.src_dir info in
             Dir_contents.get sctx ~dir
           in
           let* ocaml = Context.ocaml (Super_context.context sctx) in
           let* modules =
             let db = Scope.libs scope in
             let* preprocess =
               (* TODO wrong, this should be delayed *)
               Resolve.Memo.read_memo
                 (Preprocess.Per_module.with_instrumentation
                    lib.buildable.preprocess
                    ~instrumentation_backend:(Lib.DB.instrumentation_backend db))
             in
             let pp_spec =
               Staged.unstage (Pp_spec.pped_modules_map preprocess ocaml.version)
             in
             Dir_contents.ocaml dir_contents
             >>= Ml_sources.modules
                   ~libs:db
                   ~for_:(Library (Lib_info.lib_id info |> Lib_id.to_local_exn))
             >>= Modules.map_user_written ~f:(fun m -> Memo.return (pp_spec m))
           in
           let+ foreign_objects =
             let ext_obj = ocaml.lib_config.ext_obj in
             let dir = Obj_dir.obj_dir (Lib.Local.obj_dir vlib) in
             let+ foreign_sources = Dir_contents.foreign_sources dir_contents in
             foreign_sources
             |> Foreign_sources.for_lib ~name
             |> Foreign.Sources.object_files ~ext_obj ~dir
             |> List.map ~f:Path.build
           in
           modules, foreign_objects
       in
       Some (Vimpl.make ~impl:lib ~vlib ~vlib_modules ~vlib_foreign_objects))
;;
