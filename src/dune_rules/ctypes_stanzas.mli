(* Expand a library with a ctypes stanza into several support libraries. *)
open Dune_file

module Ctypes = Ctypes_library

val buildable :
     ?flags:Ocaml_flags.Spec.t
  -> ?foreign_stubs:Foreign.Stubs.t list
  -> loc:Stdune.Loc.t
  -> libraries:string list
  -> modules:Module_name.t list
  -> unit
  -> Buildable.t

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

val dynlink : Ctypes_library.t -> Dynlink_supported.t

val library_stanzas :
     parsing_context:Stdune.Univ_map.t
  -> project:Dune_engine.Dune_project.t
  -> ctypes_library:Ctypes_library.t
  -> Library.t list

val generated_ml_and_c_files : Ctypes.t -> string list
