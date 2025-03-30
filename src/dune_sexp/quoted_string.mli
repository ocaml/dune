open Stdune

type t =
  | Single of string
  | Multi of string list

val equal : t -> t -> bool
val to_dyn : t -> Dyn.t
val to_string : t -> string
val pp : t -> 'a Pp.t
val to_sexp : t -> Sexp.t

module Delimiter : sig
  val escaped : string
  val literal : string
end
