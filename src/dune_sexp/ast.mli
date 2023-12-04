(** Abstract syntax tree *)

open Stdune

type t =
  | Atom of Loc.t * Atom.t
  | Quoted_string of Loc.t * string
  | Template of Template.t
  | List of Loc.t * t list

val atom_or_quoted_string : Loc.t -> string -> t
val loc : t -> Loc.t
val remove_locs : t -> T.t
val add_loc : T.t -> loc:Loc.t -> t
val equal : t -> t -> bool
