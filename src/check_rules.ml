open Stdune

let dev_files p =
  match Path.extension p with
  | ".cmt"
  | ".cmti"
  | ".cmi" -> true
  | _ -> false

let add_obj_dir sctx ~obj_dir =
  if (Super_context.context sctx).merlin then
    let f dir =
      Super_context.add_alias_deps
        sctx
        (Build_system.Alias.check ~dir:obj_dir.Obj_dir.dir)
        ~dyn_deps:(Build.paths_matching ~loc:Loc.none ~dir dev_files)
        Path.Set.empty
    in
    List.iter ~f (Obj_dir.all_objs_dir obj_dir)
