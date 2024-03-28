open Import

type visibility =
  | Public of Public_lib.t
  | Private of Package.t option

type t =
  { name : Loc.t * Lib_name.Local.t
  ; visibility : visibility
  ; synopsis : string option
  ; install_c_headers : (Loc.t * string) list
  ; public_headers : Loc.t * Dep_conf.t list
  ; ppx_runtime_libraries : (Loc.t * Lib_name.t) list
  ; modes : Mode_conf.Lib.Set.t
  ; kind : Lib_kind.t
      (* TODO: It may be worth remaming [c_library_flags] to
         [link_time_flags_for_c_compiler] and [library_flags] to
         [link_time_flags_for_ocaml_compiler], both here and in the Dune
         language, to make it easier to understand the purpose of various
         flags. Also we could add [c_library_flags] to [Foreign.Stubs.t]. *)
  ; library_flags : Ordered_set_lang.Unexpanded.t
  ; c_library_flags : Ordered_set_lang.Unexpanded.t
  ; virtual_deps : (Loc.t * Lib_name.t) list
  ; wrapped : Wrapped.t Lib_info.Inherited.t
  ; optional : bool
  ; buildable : Buildable.t
  ; dynlink : Dynlink_supported.t
  ; project : Dune_project.t
  ; sub_systems : Sub_system_info.t Sub_system_name.Map.t
  ; dune_version : Dune_lang.Syntax.Version.t
  ; virtual_modules : Ordered_set_lang.Unexpanded.t option
  ; implements : (Loc.t * Lib_name.t) option
  ; default_implementation : (Loc.t * Lib_name.t) option
  ; private_modules : Ordered_set_lang.Unexpanded.t option
  ; stdlib : Ocaml_stdlib.t option
  ; special_builtin_support : (Loc.t * Lib_info.Special_builtin_support.t) option
  ; enabled_if : Blang.t
  ; instrumentation_backend : (Loc.t * Lib_name.t) option
  ; melange_runtime_deps : Loc.t * Dep_conf.t list
  }

include Stanza.S with type t := t

val decode : t Dune_lang.Decoder.t
val sub_dir : t -> string option
val package : t -> Package.t option

(** Check if the library has any foreign stubs or archives. *)
val has_foreign : t -> bool

(** Check if the library has any c++ foreign stubs. *)
val has_foreign_cxx : t -> bool

(** The foreign stubs archive. *)
val stubs_archive : t -> Foreign.Archive.t option

(** The list of foreign archives. *)
val foreign_archives : t -> Foreign.Archive.t list

(** The [lib*.a] files of all foreign archives, including foreign stubs. [dir]
    is the directory the library is declared in. Only files relevant to the
    [for_mode] selection will be returned. *)
val foreign_lib_files
  :  t
  -> dir:Path.Build.t
  -> ext_lib:string
  -> for_mode:Mode.Select.t
  -> Path.Build.t list

(** The path to a library archive. [dir] is the directory the library is
    declared in. *)
val archive : t -> dir:Path.Build.t -> ext:string -> Path.Build.t

val archive_basename : t -> ext:string -> string
val best_name : t -> Lib_name.t
val is_virtual : t -> bool
val is_impl : t -> bool
val obj_dir : dir:Path.Build.t -> t -> Path.Build.t Obj_dir.t
val main_module_name : t -> Lib_info.Main_module_name.t

val to_lib_info
  :  t
  -> expander:Expander0.t Memo.t
  -> dir:Path.Build.t
  -> lib_config:Lib_config.t
  -> Lib_info.local
