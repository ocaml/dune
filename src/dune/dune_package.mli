open! Stdune

module Lib : sig
  type t

  val dir : t -> Path.t

  val orig_src_dir : t -> Path.t option

  val obj_dir : t -> Path.t Obj_dir.t

  val requires : t -> (Loc.t * Lib_name.t) list

  val name : t -> Lib_name.t

  val version : t -> string option

  val kind : t -> Lib_kind.t

  val loc : t -> Loc.t

  val sub_systems : t -> Sub_system_info.t Sub_system_name.Map.t

  val synopsis : t -> string option

  val ppx_runtime_deps : t -> (Loc.t * Lib_name.t) list

  val foreign_objects : t -> Path.t list

  val foreign_archives : t -> Path.t list Mode.Dict.t

  val archives : t -> Path.t list Mode.Dict.t

  val virtual_ : t -> bool

  val modules : t -> Modules.t option

  val main_module_name : t -> Module_name.t option

  val plugins : t -> Path.t list Mode.Dict.t

  val jsoo_runtime : t -> Path.t list

  val implements : t -> (Loc.t * Lib_name.t) option

  val known_implementations : t -> (Loc.t * Lib_name.t) Variant.Map.t

  val default_implementation : t -> (Loc.t * Lib_name.t) option

  val special_builtin_support :
    t -> Dune_file.Library.Special_builtin_support.t option

  val dir_of_name : Lib_name.t -> Path.Local.t

  val compare_name : t -> t -> Ordering.t

  val modes : t -> Mode.Dict.Set.t

  val wrapped : t -> Wrapped.t option

  val make :
       loc:Loc.t
    -> kind:Lib_kind.t
    -> name:Lib_name.t
    -> synopsis:string option
    -> archives:Path.t list Mode.Dict.t
    -> plugins:Path.t list Mode.Dict.t
    -> foreign_objects:Path.t list
    -> foreign_archives:Path.t list Mode.Dict.t
    -> jsoo_runtime:Path.t list
    -> main_module_name:Module_name.t option
    -> sub_systems:Sub_system_info.t Sub_system_name.Map.t
    -> requires:(Loc.t * Lib_name.t) list
    -> ppx_runtime_deps:(Loc.t * Lib_name.t) list
    -> implements:(Loc.t * Lib_name.t) option
    -> default_implementation:(Loc.t * Lib_name.t) option
    -> virtual_:bool
    -> known_implementations:(Loc.t * Lib_name.t) Variant.Map.t
    -> modules:Modules.t option
    -> modes:Mode.Dict.Set.t
    -> version:string option
    -> orig_src_dir:Path.t option
    -> obj_dir:Path.t Obj_dir.t
    -> special_builtin_support:
         Dune_file.Library.Special_builtin_support.t option
    -> t

  val set_subsystems : t -> Sub_system_info.t Sub_system_name.Map.t -> t
end

type t =
  { libs : Lib.t list
  ; name : Package.Name.t
  ; version : string option
  ; dir : Path.t
  }

module Or_meta : sig
  type nonrec t =
    | Use_meta
    | Dune_package of t

  val encode : dune_version:Dune_lang.Syntax.Version.t -> t -> Dune_lang.t list

  val load : Dpath.t -> t
end
