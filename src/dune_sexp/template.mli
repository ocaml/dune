open! Stdune

module Pform : sig
  type t =
    { loc : Loc.t
    ; name : string
    ; payload : string option
    }

  val to_string : t -> string

  val to_dyn : t -> Dyn.t

  val name : t -> string

  val loc : t -> Loc.t

  val full_name : t -> string

  (** Variables do not have a payload. While macros always do. *)
  val payload : t -> string option

  val with_name : t -> name:string -> t

  (** Describe what this percent form is *)
  val describe : t -> string

  (** "macro" or "variable" *)
  val describe_kind : t -> string
end

type part =
  | Text of string
  | Pform of Pform.t

type t =
  { quoted : bool
  ; parts : part list
  ; loc : Loc.t
  }

val to_string : t -> string

val compare_no_loc : t -> t -> Ordering.t

val pp : t -> _ Pp.t

val pp_split_strings : Stdlib.Format.formatter -> t -> unit

val remove_locs : t -> t

val to_dyn : t -> Dyn.t
