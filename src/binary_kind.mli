(** Linking modes for binaries *)

open! Stdune

type t =
  | Exe
  | Object
  | Shared_object

include Dsexp.Sexpable with type t := t

val all : t list

val pp : Format.formatter -> t -> unit
