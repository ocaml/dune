open! Stdune

module Lib : sig
  type t

  val requires : t -> (Loc.t * Lib_name.t) list

  val modules : t -> Modules.t option

  val main_module_name : t -> Module_name.t option

  val known_implementations : t -> (Loc.t * Lib_name.t) Variant.Map.t

  val dir_of_name : Lib_name.t -> Path.Local.t

  val compare_name : t -> t -> Ordering.t

  val wrapped : t -> Wrapped.t option

  val info : t -> Path.t Lib_info.t

  val make :
       info:Path.t Lib_info.t
    -> main_module_name:Module_name.t option
    -> requires:(Loc.t * Lib_name.t) list
    -> known_implementations:(Loc.t * Lib_name.t) Variant.Map.t
    -> modules:Modules.t option
    -> t
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
