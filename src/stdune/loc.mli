include module type of struct include Loc0 end

val in_file : Path.t -> t

val in_dir : Path.t -> t

val none : t

val drop_position : t -> t

val of_lexbuf : Lexing.lexbuf -> t

val to_sexp : t -> Sexp.t

val sexp_of_position_no_file : Lexing.position -> Sexp.t

val equal : t -> t -> bool

(** To be used with [__POS__] *)
val of_pos : (string * int * int * int) -> t

val to_file_colon_line : t -> string
val pp_file_colon_line : Format.formatter -> t -> unit
