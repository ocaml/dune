open Import

let dev_files =
  let exts = [ Ml_kind.cmt_ext Impl; Ml_kind.cmt_ext Intf; Cm_kind.ext Cmi ] in
  let id =
    lazy
      (let open Dyn in
      variant "dev_files" (List.map ~f:string exts))
  in
  Predicate_with_id.create ~id ~f:(fun p ->
      let ext = Filename.extension p in
      List.mem exts ext ~equal:String.equal)

let add_obj_dir sctx ~obj_dir =
  if (Super_context.context sctx).merlin then
    let dir_glob =
      let dir = Path.build (Obj_dir.byte_dir obj_dir) in
      File_selector.create ~dir dev_files
    in
    Rules.Produce.Alias.add_deps
      (Alias.check ~dir:(Obj_dir.dir obj_dir))
      (Action_builder.paths_matching_unit ~loc:(Loc.of_pos __POS__) dir_glob)
  else Memo.return ()

let add_files sctx ~dir files =
  if (Super_context.context sctx).merlin then
    let alias = Alias.check ~dir in
    let files = Path.Set.of_list files in
    Rules.Produce.Alias.add_deps alias (Action_builder.path_set files)
  else Memo.return ()

let add_cycle_check sctx ~dir modules =
  if (Super_context.context sctx).merlin then
    let alias = Alias.check ~dir in
    Rules.Produce.Alias.add_deps alias (Action_builder.ignore modules)
  else Memo.return ()
