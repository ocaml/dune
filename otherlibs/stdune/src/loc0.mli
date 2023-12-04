type t

val to_lexbuf_loc : t -> Lexbuf.Loc.t
val of_lexbuf_loc : Lexbuf.Loc.t -> t
val start : t -> Lexing.position
val map_pos : t -> f:(Lexing.position -> Lexing.position) -> t
val create : start:Lexing.position -> stop:Lexing.position -> t
val in_file : fname:string -> t
val set_stop : t -> Lexing.position -> t
val set_start : t -> Lexing.position -> t
val stop : t -> Lexing.position
val compare : t -> t -> Ordering.t
val equal : t -> t -> bool
val none : t
val is_none : t -> bool
val to_dyn : t -> Dyn.t
val set_start_to_stop : t -> t
val start_pos_cnum : t -> int
val stop_pos_cnum : t -> int
