include module type of struct
  include Loc0
end

module Map : Map_intf.S with type key := t

val in_file : Path.t -> t
val in_dir : Path.t -> t
val none : t
val is_none : t -> bool
val drop_position : t -> t
val of_lexbuf : Lexing.lexbuf -> t
val to_dyn : t -> Dyn.t

(** To be used with [__POS__] *)
val of_pos : string * int * int * int -> t

val to_file_colon_line : t -> string
val pp_file_colon_line : t -> 'a Pp.t
val to_dyn_hum : t -> Dyn.t

type tag = Loc

val pp : t -> tag Pp.t
val render : Format.formatter -> tag Pp.t -> unit
val on_same_line : t -> t -> bool
val compare : t -> t -> Ordering.t
val span : t -> t -> t
val set_start_to_stop : t -> t
