(** Linking modes for binaries *)

open! Stdune

type t =
  | C
  | Exe
  | Object
  | Shared_object

include Galach.Sexpable with type t := t

val all : t list

val pp : Format.formatter -> t -> unit
