type t

val to_lexbuf_loc : t -> Lexbuf.Loc.t

val of_lexbuf_loc : Lexbuf.Loc.t -> t

val start : t -> Lexing.position

val map_pos : t -> f:(Lexing.position -> Lexing.position) -> t

val create : start:Lexing.position -> stop:Lexing.position -> t

val set_stop : t -> Lexing.position -> t

val set_start : t -> Lexing.position -> t

val stop : t -> Lexing.position

val compare : t -> t -> Ordering.t

val equal : t -> t -> bool

val none_pos : string -> Lexing.position

val none : t

val dyn_of_position_no_file : Lexing.position -> Dyn.t

val to_dyn : t -> Dyn.t
