open! Stdune

module Pform : sig
  module Payload : sig
    type t

    val of_string : string -> t
    val to_string : t -> string
    val to_dyn : t -> Dyn.t
    val compare : t -> t -> Ordering.t
    val of_args : string list -> t

    module Args : sig
      (** Treat the entire payload as a single string argument. *)
      val whole : t -> string

      (** Split the payload on the first ':' character returning [None] if the
          payload contains no ':' characters. The [loc] argument is used for
          error reporting. *)
      val lsplit2 : t -> Loc.t -> (string * string, User_message.t) result

      (** Split the payload on all ':' characters *)
      val split : t -> string list
    end
  end

  type t =
    { loc : Loc.t
    ; name : string
    ; payload : Payload.t option
    }

  val compare : t -> t -> Ordering.t
  val to_string : t -> string
  val to_dyn : t -> Dyn.t
  val name : t -> string
  val loc : t -> Loc.t
  val payload_loc : t -> Loc.t
  val full_name : t -> string

  (** Variables do not have a payload. While macros always do. *)
  val payload : t -> Payload.t option

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
val compare : t -> t -> Ordering.t
val pp : t -> _ Pp.t
val pp_split_strings : Stdlib.Format.formatter -> t -> unit
val remove_locs : t -> t
val to_dyn : t -> Dyn.t
