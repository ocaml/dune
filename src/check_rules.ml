open Stdune

let dev_files p =
  match Path.extension p with
  | ".cmt"
  | ".cmti"
  | ".cmi" -> true
  | _ -> false

let add_obj_dir sctx ~obj_dir =
  if (Super_context.context sctx).merlin then
    Build_system.Alias.add_deps
      (Build_system.Alias.check ~dir:(Obj_dir.dir obj_dir))
      ~dyn_deps:(Build.paths_matching
                   ~loc:(Loc.of_pos __POS__)
                   ~dir:(Obj_dir.byte_dir obj_dir) dev_files)
      Path.Set.empty
