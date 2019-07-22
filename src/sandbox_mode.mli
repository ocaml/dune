open! Stdune

type some =
  | Symlink
  | Copy

type t = some option

val all : t list

val none : t
val symlink : t
val copy : t

val of_string : string -> (t, string) Result.t
val to_string : t -> string
