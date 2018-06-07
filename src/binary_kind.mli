(** Linking modes for binaries *)

open Stdune

type t =
  | Exe
  | Object
  | Shared_object

val t : t Sexp.Of_sexp.t

val sexp_of_t : t Sexp.To_sexp.t

val all : t list

val pp : Format.formatter -> t -> unit
