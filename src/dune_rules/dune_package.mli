(** Representation of dune-package files *)

open Import

(** The filename of a dune-package file*)
val fn : string

module Lib : sig
  type t

  val main_module_name : t -> Module_name.t option

  val dir_of_name : Lib_name.t -> Path.Local.t

  val wrapped : t -> Wrapped.t option

  val info : t -> Path.t Lib_info.t

  val of_findlib : Path.t Lib_info.t -> t

  val of_dune_lib :
    info:Path.t Lib_info.t -> main_module_name:Module_name.t option -> t

  val to_dyn : t Dyn.builder
end

module Deprecated_library_name : sig
  type t =
    { loc : Loc.t
    ; old_public_name : Lib_name.t
    ; new_public_name : Lib_name.t
    }

  val to_dyn : t Dyn.builder
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

  val to_dyn : t Dyn.builder
end

type t =
  { name : Package.Name.t
  ; entries : Entry.t Lib_name.Map.t
  ; version : string option
  ; sections : Path.t Section.Map.t
  ; sites : Section.t Section.Site.Map.t
  ; dir : Path.t
  ; files : (Section.t * Install.Dst.t list) list
  }

val to_dyn : t Dyn.builder

module Or_meta : sig
  type nonrec t =
    | Use_meta
    | Dune_package of t

  val pp :
    dune_version:Dune_lang.Syntax.Version.t -> Format.formatter -> t -> unit

  val load : Dpath.t -> t Or_exn.t Memo.t

  val to_dyn : t Dyn.builder
end
