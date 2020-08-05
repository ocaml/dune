(** Link mode of OCaml programs *)

type t =
  | Byte
  | Native
  | Byte_with_stubs_statically_linked_in

val mode : t -> Mode.t
