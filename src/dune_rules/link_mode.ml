open! Dune_engine
open! Stdune

type t =
  | Byte
  | Native
  | Byte_with_stubs_statically_linked_in

let mode : t -> Mode.t = function
  | Byte -> Byte
  | Native -> Native
  | Byte_with_stubs_statically_linked_in -> Byte

let equal x y =
  match (x, y) with
  | Byte, Byte -> true
  | Byte, _ -> false
  | _, Byte -> false
  | Native, Native -> true
  | Native, _ -> false
  | _, Native -> false
  | Byte_with_stubs_statically_linked_in, Byte_with_stubs_statically_linked_in
    ->
    true
