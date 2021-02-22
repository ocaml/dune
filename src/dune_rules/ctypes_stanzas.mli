(* Expand a library with a ctypes stanza into several support libraries. *)
open Dune_file

val type_description_module : Ctypes.t -> Module_name.t
val type_description_library : Ctypes.t -> string

val function_description_module : Ctypes.t -> Module_name.t
val function_description_library : Ctypes.t -> string

val cflags_sexp : Ctypes.t -> string
val c_library_flags_sexp : Ctypes.t -> string
val c_generated_types_module : Ctypes.t -> Module_name.t
val c_generated_functions_module : Ctypes.t -> Module_name.t
val entry_module : Ctypes.t -> Module_name.t

val c_types_includer_module : Ctypes.t -> Module_name.t

val c_generated_types_cout_c : Ctypes.t -> string
val c_generated_types_cout_exe : Ctypes.t -> string

val c_generated_functions_cout_c : Ctypes.t -> string

val library_stanzas :
     parsing_context:Stdune.Univ_map.t
  -> project:Dune_engine.Dune_project.t
  -> sub_systems:Sub_system_info.t Sub_system_name.Map.t
  -> dune_version:Dune_lang.Syntax.Version.t
  -> Buildable.t
  -> Library.t list

val generated_ml_and_c_files : Ctypes.t -> string list
