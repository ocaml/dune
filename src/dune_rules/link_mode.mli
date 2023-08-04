(** Link mode of OCaml programs *)
open! Import

type t =
  | Byte
  | Byte_for_jsoo
  | Native
  | Byte_with_stubs_statically_linked_in

val mode : t -> Mode.t
val equal : t -> t -> bool
