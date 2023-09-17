open! Stdune
open Dune_sexp

type t = Dune_glob.V1.t

val equal : t -> t -> bool
val compare : t -> t -> Ordering.t
val hash : t -> int
val to_dyn : t Dyn.builder
val encode : t Encoder.t
val decode : t Decoder.t
val test : t -> string -> bool
val filter : t -> string list -> string list
val empty : t
val universal : t
val of_string_exn : Loc.t -> string -> t
val of_string : string -> t
val to_predicate : t -> Filename.t Predicate.t
val matching_extensions : Filename.Extension.t list -> t
