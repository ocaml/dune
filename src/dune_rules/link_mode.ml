open Import

type t =
  | Byte
  | Byte_for_jsoo
  | Native
  | Byte_with_stubs_statically_linked_in

let mode : t -> Mode.t = function
  | Byte -> Byte
  | Byte_for_jsoo -> Byte
  | Native -> Native
  | Byte_with_stubs_statically_linked_in -> Byte
;;

let equal x y =
  match x, y with
  | Byte, Byte -> true
  | Byte, _ -> false
  | _, Byte -> false
  | Byte_for_jsoo, Byte_for_jsoo -> true
  | Byte_for_jsoo, _ -> false
  | _, Byte_for_jsoo -> false
  | Native, Native -> true
  | Native, _ -> false
  | _, Native -> false
  | Byte_with_stubs_statically_linked_in, Byte_with_stubs_statically_linked_in -> true
;;
