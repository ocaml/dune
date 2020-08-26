(** Representation of dune-package files *)
open! Dune_engine

open! Stdune

(** The filename of a dune-package file*)
val fn : string

module Lib : sig
  type t

  val modules : t -> Modules.t option

  val main_module_name : t -> Module_name.t option

  val dir_of_name : Lib_name.t -> Path.Local.t

  val wrapped : t -> Wrapped.t option

  val info : t -> Path.t Lib_info.t

  val make :
       info:Path.t Lib_info.t
    -> main_module_name:Module_name.t option
    -> modules:Modules.t option
    -> t

  val to_dyn : t Dyn.Encoder.t
end

module Deprecated_library_name : sig
  type t =
    { loc : Loc.t
    ; old_public_name : Lib_name.t
    ; new_public_name : Lib_name.t
    }

  val to_dyn : t Dyn.Encoder.t
end

module Entry : sig
  type t =
    | Library of Lib.t
    | Deprecated_library_name of Deprecated_library_name.t
    | Hidden_library of Lib.t
        (** Only for external libraries that:

            - are not built with dune

            - have a [META] file with an unsatisfied [exist_if] field

            Dune itself never produces hidden libraries. *)

  val name : t -> Lib_name.t

  val version : t -> string option

  val loc : t -> Loc.t

  val to_dyn : t Dyn.Encoder.t
end

type t =
  { name : Package.Name.t
  ; entries : Entry.t Lib_name.Map.t
  ; version : string option
  ; dir : Path.t
  }

val to_dyn : t Dyn.Encoder.t

module Or_meta : sig
  type nonrec t =
    | Use_meta
    | Dune_package of t

  val encode : dune_version:Dune_lang.Syntax.Version.t -> t -> Dune_lang.t list

  val pp :
    dune_version:Dune_lang.Syntax.Version.t -> Format.formatter -> t -> unit

  val load : Dpath.t -> t Or_exn.t

  val to_dyn : t Dyn.Encoder.t
end
