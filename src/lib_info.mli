(** {1 Raw library descriptions} *)

open Stdune

module Status : sig
  type t =
    | Installed
    | Public  of Dune_project.Name.t * Package.t
    | Private of Dune_project.Name.t

  val pp : t Fmt.t

  val is_private : t -> bool

  (** For local libraries, return the project name they are part of *)
  val project_name : t -> Dune_project.Name.t option
end

module Deps : sig
  type t =
    | Simple  of (Loc.t * Lib_name.t) list
    | Complex of Dune_file.Lib_dep.t list

  val of_lib_deps : Dune_file.Lib_deps.t -> t
end

(** For values like modules that need to be evaluated to be fetched *)
module Source : sig
  type 'a t =
    | Local
    | External of 'a
end

module Enabled_status : sig
  type t =
    | Normal
    | Optional
    | Disabled_because_of_enabled_if
end

type 'path t = private
  { loc              : Loc.t
  ; name             : Lib_name.t
  ; kind             : Lib_kind.t
  ; status           : Status.t
  ; src_dir          : 'path
  ; orig_src_dir     : 'path option
  ; obj_dir          : 'path Obj_dir.t
  ; version          : string option
  ; synopsis         : string option
  ; archives         : 'path list Mode.Dict.t
  ; plugins          : 'path list Mode.Dict.t
  ; foreign_objects  : 'path list Source.t
  ; foreign_archives : 'path list Mode.Dict.t (** [.a/.lib/...] files *)
  ; jsoo_runtime     : 'path list
  ; jsoo_archive     : 'path option
  ; requires         : Deps.t
  ; ppx_runtime_deps : (Loc.t * Lib_name.t) list
  ; pps              : (Loc.t * Lib_name.t) list
  ; enabled          : Enabled_status.t
  ; virtual_deps     : (Loc.t * Lib_name.t) list
  ; dune_version     : Syntax.Version.t option
  ; sub_systems      : Sub_system_info.t Sub_system_name.Map.t
  ; virtual_         : Lib_modules.t Source.t option
  ; implements       : (Loc.t * Lib_name.t) option
  ; variant          : Variant.t option
  ; known_implementations : (Loc.t * Lib_name.t) Variant.Map.t
  ; default_implementation  : (Loc.t * Lib_name.t) option
  ; wrapped          : Wrapped.t Dune_file.Library.Inherited.t option
  ; main_module_name : Dune_file.Library.Main_module_name.t
  ; modes            : Mode.Dict.Set.t
  ; special_builtin_support : Dune_file.Library.Special_builtin_support.t option
  }

type external_ = Path.t t
type local = Path.Build.t t

val of_library_stanza
  :  dir:Path.Build.t
  -> lib_config:Lib_config.t
  -> (Loc.t * Lib_name.t) Variant.Map.t
  -> Dune_file.Library.t
  -> local

val user_written_deps : _ t -> Dune_file.Lib_deps.t

val of_dune_lib
  :  Sub_system_info.t Dune_package.Lib.t
  -> external_

val to_external : local -> external_

(* CR-someday diml: this should be [Path.t list], since some libraries
   have multiple source directories because of [copy_files]. *)
(** Directory where the source files for the library are located. *)
val orig_src_dir : 'path t -> 'path
