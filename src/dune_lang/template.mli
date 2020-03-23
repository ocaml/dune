open! Stdune

type var =
  { loc : Loc.t
  ; name : string
  ; payload : string option
  }

type part =
  | Text of string
  | Var of var

type t =
  { quoted : bool
  ; parts : part list
  ; loc : Loc.t
  }

val to_string : t -> string

val compare_no_loc : t -> t -> Ordering.t

val string_of_var : var -> string

val pp : t -> _ Pp.t

val pp_split_strings : Format.formatter -> t -> unit

val remove_locs : t -> t

val to_dyn : t -> Dyn.t
