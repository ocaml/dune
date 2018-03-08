(** Linking modes for binaries *)

type t =
  | Exe
  | Object
  | Shared_object

val t : t Sexp.Of_sexp.t

val all : t list
