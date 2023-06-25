(** Positions information that can be encoded within a single immediate *)

type t [@@immediate]

val of_position : Lexbuf.Position.t -> t
val to_position : t -> fname:string -> Lexbuf.Position.t
val lnum : t -> int
val cnum : t -> int
val bol : t -> int

module Same_line_loc : sig
  type t [@@immediate]

  val lnum : t -> int
  val bol : t -> int
  val start_cnum : t -> int
  val stop_cnum : t -> int
  val to_loc : t -> fname:string -> Lexbuf.Loc.t
  val start : t -> fname:string -> Lexbuf.Position.t
  val stop : t -> fname:string -> Lexbuf.Position.t
  val set_start_to_stop : t -> t
end

type of_loc =
  | Same_line of Same_line_loc.t
  | Loc of
      { start : t
      ; stop : t
      }
  | Loc_does_not_fit

val of_loc : Lexbuf.Loc.t -> of_loc

module For_tests : sig
  val small_enough : Lexbuf.Position.t -> bool
end
