open Stdune

(** Re-export Pform from Part module for backward compatibility *)
module Pform = Part.Pform

(** Re-export Part from Part module for backward compatibility *)
module Part = Part

type t =
  { quoted : bool
  ; parts : Part.t list
  ; loc : Loc.t
  }

val to_string : t -> string
val equal : t -> t -> bool
val pp : t -> _ Pp.t
val remove_locs : t -> t
val to_dyn : t -> Dyn.t
