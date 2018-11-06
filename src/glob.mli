open! Stdune

type t

val decode : t Stanza.Decoder.t

val test : t -> string -> bool

val filter : t -> string list -> string list

val empty : t
