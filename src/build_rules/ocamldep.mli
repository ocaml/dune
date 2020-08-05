(** ocamldep management *)

open Stdune

val deps_of :
     cctx:Compilation_context.t
  -> ml_kind:Ml_kind.t
  -> Module.t
  -> Module.t list Build.t

val read_deps_of :
     obj_dir:Path.Build.t Obj_dir.t
  -> modules:Modules.t
  -> ml_kind:Ml_kind.t
  -> Module.t
  -> Module.t list Build.t
