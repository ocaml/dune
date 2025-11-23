open Import

val gen_rules
  :  Super_context.t
  -> Ocaml_toolchain.t
  -> Loc.t
  -> obj_dir:Path.Build.t Obj_dir.t
  -> modules:Modules.With_vlib.t
  -> dir:Path.Build.t
  -> direct_requires:(Loc.t * Lib.t) list Resolve.Memo.t
  -> allow_unused_libraries:Lib.t list Resolve.Memo.t
  -> unit Memo.t
