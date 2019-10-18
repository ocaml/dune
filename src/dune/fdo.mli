(** Integration with feedback-directed optimizations using ocamlfdo. *)

type phase =
  | All
  | Compile
  | Emit

val linear_ext : string

val linear_fdo_ext : string

val phase_flags : phase option -> string list

val opt_rule : Compilation_context.t -> Module.t -> string -> unit
