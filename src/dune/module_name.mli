open Stdune

(** Represents a valid OCaml module name *)
type t

val to_dyn : t -> Dyn.t
include Dune_lang.Conv with type t := t

val add_suffix : t -> string -> t

val compare : t -> t -> Ordering.t
val of_string : string -> t
val to_string : t -> string

val uncapitalize : t -> string

val pp_quote : Format.formatter -> t -> unit

module Set : sig
  include Set.S with type elt = t
  val to_dyn : t -> Dyn.t
end

module Map : Map.S with type key = t

module Infix : Comparator.OPS with type t = t

val of_local_lib_name : Lib_name.Local.t -> t

val to_local_lib_name : t -> Lib_name.Local.t
