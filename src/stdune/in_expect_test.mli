(** A [printf] that's usable in expect tests. *)

val formatter : Format.formatter option ref

val printf : ('a, Format.formatter, unit) format -> 'a
