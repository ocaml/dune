type t = Loc0.t =
  { start : Lexing.position
  ; stop  : Lexing.position
  }

val in_file : string -> t

val none : t

val of_lexbuf : Lexing.lexbuf -> t

val sexp_of_t : t -> Sexp0.t

val sexp_of_position_no_file : Lexing.position -> Sexp0.t

val equal : t -> t -> bool

(** To be used with [__POS__] *)
val of_pos : (string * int * int * int) -> t

val to_file_colon_line : t -> string
val pp_file_colon_line : Format.formatter -> t -> unit
