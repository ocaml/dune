(** Raw library descriptions *)

open Import

(** This module regroup all information about a library. We call such
    descriptions "raw" as the names, such as the names of dependencies are plain
    unresolved library names.

    The [Lib] module takes care of resolving library names to actual libraries. *)

module Status : sig
  type t =
    | Installed_private
    | Installed
    | Public of Dune_project.t * Package.t
    | Private of Dune_project.t * Package.t option

  val is_private : t -> bool

  (** For local libraries, return the project they are part of *)
  val project : t -> Dune_project.t option

  (** [relative_to_package t name] return the path of [name] relative to the
      package determined by [t]. If there's no package, return [None] *)
  val relative_to_package : t -> Lib_name.t -> Path.Local.t option
end

(** For values like modules that need to be evaluated to be fetched *)
module Source : sig
  type 'a t =
    | Local
    | External of 'a
end

module File_deps : sig
  type 'a t =
    | Local of Loc.t * Dep_conf.t list
    | External of 'a list
end

module Enabled_status : sig
  type t =
    | Normal
    | Optional
    | Disabled_because_of_enabled_if
end

module Special_builtin_support : sig
  module Build_info : sig
    type api_version = V1

    type t =
      { data_module : Module_name.t
      ; api_version : api_version
      }
  end

  module Configurator : sig
    type api_version = V1
    type t = { api_version : api_version }
  end

  module Dune_site : sig
    type t =
      { data_module : Module_name.t
      ; plugins : bool
      }
  end

  type t =
    | Findlib_dynload
    | Build_info of Build_info.t
    | Configurator of Configurator.t
    | Dune_site of Dune_site.t

  include Dune_lang.Conv.S with type t := t
end

module Inherited : sig
  type 'a t =
    | This of 'a
    | From of (Loc.t * Lib_name.t)
end

module Main_module_name : sig
  type t = Module_name.t option Inherited.t
end

type 'path t

val name : _ t -> Lib_name.t
val lib_id : _ t -> Lib_id.t
val loc : _ t -> Loc.t

(** The [*.cma] and [*.cmxa] files for OCaml libraries. Libraries built by Dune
    will always have zero or one element in the list (zero if they are not
    buildable in the corresponding mode). External libraries, however, can have
    more than one element in the list, because the format allows for that. *)
val archives : 'path t -> 'path list Mode.Dict.t

(* TODO: Rename [foreign_archives] to [foreign_lib_files] and [native_archives]
   to [native_lib_files] for consistent naming with [foreign_dll_files]. *)

(** All the [lib*.a] files for stubs. A table indexed by [Mode.Select.t] is used
    to account for mode-dependent archives. The special key [Mode.Select.All] is
    used for archives that should be built in every modes. *)
val foreign_archives : 'path t -> 'path Mode.Map.Multi.t

type 'path native_archives =
  | Needs_module_info of 'path
  | Files of 'path list

(** The [lib*.a] files for the OCaml code when compiling to native mode *)
val native_archives : 'path t -> 'path native_archives

(** [eval_native_archives] is like [native_archives] but it knows how to
    evaluate [Needs_module_info] into the list of archives *)
val eval_native_archives_exn : 'path t -> modules:Modules.With_vlib.t option -> 'path list

(** [dll*.so] files for stubs. These are read when linking a bytecode executable
    and are loaded dynamically at runtime by bytecode executables. *)
val foreign_dll_files : 'path t -> 'path list

val foreign_objects : 'path t -> 'path list Source.t
val public_headers : 'path t -> 'path File_deps.t

(** The library has a module that must be linked at the end. This is used for
    the [Std_exit] module of the stdlib. *)
val exit_module : _ t -> Module_name.t option

val instrumentation_backend : _ t -> (Loc.t * Lib_name.t) option
val plugins : 'path t -> 'path list Mode.Dict.t
val src_dir : 'path t -> 'path
val status : _ t -> Status.t
val default_implementation : _ t -> (Loc.t * Lib_name.t) option
val kind : _ t -> Lib_kind.t
val synopsis : _ t -> string option
val jsoo_runtime : 'path t -> 'path list
val wasmoo_runtime : 'path t -> 'path list
val melange_runtime_deps : 'path t -> 'path File_deps.t
val obj_dir : 'path t -> 'path Obj_dir.t
val virtual_ : _ t -> Modules.t Source.t option
val entry_modules : _ t -> (Module_name.t list, User_message.t) result Source.t
val main_module_name : _ t -> Main_module_name.t
val wrapped : _ t -> Wrapped.t Inherited.t option
val special_builtin_support : _ t -> (Loc.t * Special_builtin_support.t) option
val modes : _ t -> Lib_mode.Map.Set.t
val modules : _ t -> Modules.With_vlib.t option Source.t
val implements : _ t -> (Loc.t * Lib_name.t) option
val requires : _ t -> Lib_dep.t list
val ppx_runtime_deps : _ t -> (Loc.t * Lib_name.t) list
val preprocess : _ t -> Preprocess.With_instrumentation.t Preprocess.Per_module.t
val sub_systems : _ t -> Sub_system_info.t Sub_system_name.Map.t
val enabled : _ t -> Enabled_status.t Memo.t
val orig_src_dir : 'path t -> 'path option
val version : _ t -> Package_version.t option
val dune_version : _ t -> Dune_lang.Syntax.Version.t option
val dynlink_supported : _ t -> bool

(** Directory where the source files for the library are located. Returns the
    original src dir when it exists *)
val best_src_dir : 'path t -> 'path

type external_ = Path.t t
type local = Path.Build.t t

val user_written_deps : _ t -> Lib_dep.t list
val of_local : local -> external_
val as_local_exn : external_ -> local
val set_version : 'a t -> Package_version.t option -> 'a t

val for_dune_package
  :  Path.t t
  -> name:Lib_name.t
  -> ppx_runtime_deps:(Loc.t * Lib_name.t) list
  -> requires:Lib_dep.t list
  -> foreign_objects:Path.t list
  -> obj_dir:Path.t Obj_dir.t
  -> implements:(Loc.t * Lib_name.t) option
  -> default_implementation:(Loc.t * Lib_name.t) option
  -> sub_systems:Sub_system_info.t Sub_system_name.Map.t
  -> melange_runtime_deps:Path.t list
  -> public_headers:Path.t list
  -> modules:Modules.With_vlib.t
  -> Path.t t

val map_path : Path.t t -> f:(Path.t -> Path.t) -> Path.t t

type 'a path =
  | Local : Path.Build.t path
  | External : Path.t path

val create
  :  loc:Loc.t
  -> path_kind:'a path
  -> name:Lib_name.t
  -> lib_id:Lib_id.t
  -> kind:Lib_kind.t
  -> status:Status.t
  -> src_dir:'a
  -> orig_src_dir:'a option
  -> obj_dir:'a Obj_dir.t
  -> version:Package_version.t option
  -> synopsis:string option
  -> main_module_name:Main_module_name.t
  -> sub_systems:Sub_system_info.t Sub_system_name.Map.t
  -> requires:Lib_dep.t list
  -> foreign_objects:'a list Source.t
  -> public_headers:'a File_deps.t
  -> plugins:'a list Mode.Dict.t
  -> archives:'a list Mode.Dict.t
  -> ppx_runtime_deps:(Loc.t * Lib_name.t) list
  -> foreign_archives:'a Mode.Map.Multi.t
  -> native_archives:'a native_archives
  -> foreign_dll_files:'a list
  -> jsoo_runtime:'a list
  -> wasmoo_runtime:'a list
  -> preprocess:Preprocess.With_instrumentation.t Preprocess.Per_module.t
  -> enabled:Enabled_status.t Memo.t
  -> virtual_deps:(Loc.t * Lib_name.t) list
  -> dune_version:Dune_lang.Syntax.Version.t option
  -> virtual_:Modules.t Source.t option
  -> entry_modules:(Module_name.t list, User_message.t) result Source.t
  -> implements:(Loc.t * Lib_name.t) option
  -> default_implementation:(Loc.t * Lib_name.t) option
  -> modes:Lib_mode.Map.Set.t
  -> modules:Modules.With_vlib.t option Source.t
  -> wrapped:Wrapped.t Inherited.t option
  -> special_builtin_support:(Loc.t * Special_builtin_support.t) option
  -> exit_module:Module_name.t option
  -> instrumentation_backend:(Loc.t * Lib_name.t) option
  -> melange_runtime_deps:'a File_deps.t
  -> 'a t

val package : _ t -> Package.Name.t option
val to_dyn : 'path Dyn.builder -> 'path t Dyn.builder
