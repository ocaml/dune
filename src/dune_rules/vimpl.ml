open Import

type t =
  { vlib : Lib.t
  ; impl : Library.t
  ; vlib_modules : Modules.t
  ; vlib_foreign_objects : Path.t list
  ; impl_cm_kind : Cm_kind.t
  }

let vlib_modules t = t.vlib_modules
let vlib t = t.vlib
let impl t = t.impl
let impl_cm_kind t = t.impl_cm_kind
let vlib_stubs_o_files t = t.vlib_foreign_objects
let vlib_obj_map t = Modules.obj_map t.vlib_modules

let make ~sctx ~scope ~(lib : Library.t) ~info ~vlib ~for_ =
  let open Memo.O in
  let+ vlib_modules, vlib_foreign_objects =
    match Lib_info.modules info ~for_, Lib_info.foreign_objects info with
    | External modules, External fa ->
      let modules = Option.value_exn modules in
      Memo.return (Modules.With_vlib.drop_vlib modules, fa)
    | External _, Local | Local, External _ -> assert false
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
          Instrumentation.with_instrumentation
            lib.buildable.preprocess
            ~instrumentation_backend:(Lib.DB.instrumentation_backend db)
          |> Resolve.Memo.read_memo
        in
        Dir_contents.ml dir_contents ~for_
        >>= Ml_sources.modules
              ~libs:db
              ~for_:(Library (Lib_info.lib_id info |> Lib_id.to_local_exn))
        >>=
        let pp_spec =
          Staged.unstage (Pp_spec.pped_modules_map preprocess ocaml.version)
        in
        Modules.map_user_written ~f:(fun m -> Memo.return (pp_spec m))
      in
      let+ foreign_objects =
        Dir_contents.foreign_sources dir_contents
        >>| Foreign_sources.for_lib ~name
        >>| (let ext_obj = ocaml.lib_config.ext_obj in
             let dir = Obj_dir.obj_dir (Lib.Local.obj_dir vlib) in
             Foreign.Sources.object_files ~ext_obj ~dir)
        >>| List.map ~f:Path.build
      in
      modules, foreign_objects
  in
  let impl_cm_kind =
    let vlib_info = Lib.info vlib in
    let { Lib_mode.Map.ocaml = { byte; native = _ }; melange = _ } =
      Lib_info.modes vlib_info
    in
    Mode.cm_kind (if byte then Byte else Native)
  in
  { impl = lib; impl_cm_kind; vlib; vlib_modules; vlib_foreign_objects }
;;

let setup_copy_rules ~sctx ~dir vimpl =
  let open Memo.O in
  let ctx = Super_context.context sctx in
  let impl_obj_dir = Library.obj_dir ~dir vimpl.impl in
  let vlib_obj_dir = Lib.info vimpl.vlib |> Lib_info.obj_dir in
  let add_rule = Super_context.add_rule sctx ~dir in
  let copy_to_obj_dir ~src ~dst =
    add_rule ~loc:(Loc.of_pos __POS__) (Action_builder.symlink ~src ~dst)
  in
  let* { Lib_config.has_native; ext_obj; _ } =
    let+ ocaml = Context.ocaml ctx in
    ocaml.lib_config
  in
  let { Lib_mode.Map.ocaml = { byte; native }; melange } =
    Mode_conf.Lib.Set.eval vimpl.impl.modes ~has_native
  in
  let copy_obj_file m kind =
    let src = Obj_dir.Module.cm_file_exn vlib_obj_dir m ~kind in
    let dst = Obj_dir.Module.cm_file_exn impl_obj_dir m ~kind in
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
      >>> Memo.when_ melange (fun () -> copy_obj_file src (Melange Cmj))
      >>> Memo.when_ native (fun () ->
        copy_obj_file src (Ocaml Cmx)
        >>>
        let object_file dir = Obj_dir.Module.o_file_exn dir src ~ext_obj in
        copy_to_obj_dir ~src:(object_file vlib_obj_dir) ~dst:(object_file impl_obj_dir)))
  in
  let vlib_modules = vlib_modules vimpl in
  Modules.fold vlib_modules ~init:(Memo.return ()) ~f:(fun m acc -> acc >>> copy_objs m)
;;
