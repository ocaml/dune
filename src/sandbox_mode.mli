open! Stdune

type some =
  | Symlink
  | Copy

type t = some option

val all : t list

val of_string : string -> (t, string) Result.t
