open Stdune

type t =
  | Not_supported
  | Zero_terminated_strings of string

let registry = Table.create (module Path) 128

let get ~prog =
  Option.value (Table.find registry prog) ~default:Not_supported

let set ~prog t =
  Table.set registry prog t
