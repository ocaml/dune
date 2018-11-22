open Stdune

let dev_files p =
  match Path.extension p with
  | ".cmt"
  | ".cmti"
  | ".cmi" -> true
  | _ -> false

let add_obj_dir sctx ~rctx ~dir ~obj_dir =
  if (Super_context.context sctx).merlin then
    Rule_context.add_alias_deps
      rctx
      (Build_system.Alias.check ~dir)
      ~dyn_deps:(Build.paths_matching ~loc:Loc.none ~dir:obj_dir dev_files)
      Path.Set.empty
