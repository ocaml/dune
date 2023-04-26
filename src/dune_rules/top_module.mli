open Import

type lib_or_exe =
  { stanza :
      [ `Executables of Dune_file.Executables.t
      | `Library of Dune_file.Library.t
      ]
  ; module_name : Module_name.t
  ; dir : Path.Build.t
  ; dir_contents : Dir_contents.t
  }

val find_lib_or_exe :
  Super_context.t -> Path.Source.t -> lib_or_exe option Memo.t

val find_module :
     Super_context.t
  -> Path.Source.t
  -> (Module.t * Compilation_context.t * Merlin.t) option Memo.t

val private_obj_dir : Context.t -> Path.Source.t -> Path.Build.t Obj_dir.t

val gen_rules :
  Super_context.t -> dir:Path.Build.t -> comps:string list -> unit Memo.t
