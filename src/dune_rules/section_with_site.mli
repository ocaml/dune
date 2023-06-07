(** Location for installation, containing the sections relative to the current
    package, and sites of possibly other packages *)

open Import

type t =
  | Section of Section.t
  | Site of
      { pkg : Package.Name.t
      ; site : Site.t
      ; loc : Loc.t
      }

val to_string : t -> string

include Dune_lang.Conv.S with type t := t

val to_dyn : t -> Dyn.t
