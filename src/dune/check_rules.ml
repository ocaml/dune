open Stdune

let dev_files =
  let exts = [ Ml_kind.cmt_ext Impl; Ml_kind.cmt_ext Intf; Cm_kind.ext Cmi ] in
  let id =
    lazy
      (let open Dyn.Encoder in
      constr "dev_files" (List.map ~f:string exts))
  in
  Predicate.create ~id ~f:(fun p ->
      let ext = Filename.extension p in
      List.mem ext ~set:exts)

let add_obj_dir sctx ~obj_dir =
  if (Super_context.context sctx).merlin then
    let dir_glob =
      let dir = Path.build (Obj_dir.byte_dir obj_dir) in
      File_selector.create ~dir dev_files
    in
    let dyn_deps = Build.paths_matching ~loc:(Loc.of_pos __POS__) dir_glob in
    Rules.Produce.Alias.add_deps
      (Alias.check ~dir:(Obj_dir.dir obj_dir))
      ~dyn_deps Path.Set.empty

let add_files sctx ~dir files =
  if (Super_context.context sctx).merlin then
    let alias = Alias.check ~dir in
    let files = Path.Set.of_list files in
    Rules.Produce.Alias.add_deps alias files
