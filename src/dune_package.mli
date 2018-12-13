open! Stdune

module Lib : sig
  type 'sub_system t

  val dir : _ t -> Path.t
  val requires : _ t -> (Loc.t * Lib_name.t) list
  val name : _ t -> Lib_name.t
  val version : _ t -> string option
  val kind : _ t -> Lib_kind.t
  val loc : _ t -> Loc.t
  val sub_systems : 'a t -> 'a Sub_system_name.Map.t
  val synopsis : _ t -> string option
  val ppx_runtime_deps : _ t -> (Loc.t * Lib_name.t) list
  val foreign_objects : _ t -> Path.t list
  val foreign_archives : _ t -> Path.t list Mode.Dict.t
  val archives : _ t -> Path.t list Mode.Dict.t
  val virtual_ : _ t -> bool
  val modules : _ t -> Lib_modules.t option
  val main_module_name : _ t -> Module.Name.t option
  val plugins : _ t -> Path.t list Mode.Dict.t
  val jsoo_runtime : _ t -> Path.t list
  val implements : _ t -> (Loc.t * Lib_name.t) option

  val dir_of_name : Lib_name.t -> Path.Local.t

  val compare_name : _ t -> _ t -> Ordering.t

  val make
    :  loc:Loc.t
    -> kind:Lib_kind.t
    -> name:Lib_name.t
    -> synopsis:string option
    -> archives:Path.t list Mode.Dict.t
    -> plugins:Path.t list Mode.Dict.t
    -> foreign_objects:Path.t list
    -> foreign_archives:Path.t list Mode.Dict.t
    -> jsoo_runtime:Path.t list
    -> main_module_name:Module.Name.t option
    -> sub_systems:'a Sub_system_name.Map.t
    -> requires:(Loc.t * Lib_name.t) list
    -> ppx_runtime_deps:(Loc.t * Lib_name.t) list
    -> implements:(Loc.t * Lib_name.t) option
    -> virtual_:bool
    -> modules:Lib_modules.t option
    -> version:string option
    -> dir:Path.t
    -> 'a t

  val set_subsystems : 'a t -> 'b Sub_system_name.Map.t -> 'b t
end

type 'sub_system t =
  { libs         : 'sub_system Lib.t list
  ; name         : Package.Name.t
  ; version      : string option
  ; dir          : Path.t
  }

module Or_meta : sig
  type nonrec 'sub_system t =
    | Use_meta
    | Dune_package of 'sub_system t

  val encode
    :  dune_version:Syntax.Version.t
    -> (Syntax.Version.t * Dune_lang.t list) t
    -> Dune_lang.t list

  val load : Path_dune_lang.t -> Sub_system_info.t t
end
