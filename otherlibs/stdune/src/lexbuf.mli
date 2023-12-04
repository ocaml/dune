(** Lexing buffer utilities *)

type t = Lexing.lexbuf

module Position : sig
  type t = Lexing.position

  val equal : t -> t -> bool
  val in_file : fname:string -> t
  val none : t
  val to_dyn : t -> Dyn.t
  val to_dyn_no_file : t -> Dyn.t
end

module Loc : sig
  type t =
    { start : Lexing.position
    ; stop : Lexing.position
    }

  val to_dyn : t -> Dyn.t
  val compare : t -> t -> Ordering.t
  val equal : t -> t -> bool
  val map_pos : t -> f:(Position.t -> Position.t) -> t
  val in_file : fname:string -> t
  val is_file_only : t -> bool

  (** To be used with [__POS__] *)
  val of_pos : string * int * int * int -> t

  val none : t
end

(** Same as [Lexing.from_xxx] but also initialise the location to the beginning
    of the given file *)
val from_string : string -> fname:string -> t

val from_channel : in_channel -> fname:string -> t
