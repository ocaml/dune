open Stdune

type t

val is_default : t -> bool

val build_dir : t -> Path.Build.t

val default : t

val hash : t -> int

val target : t -> toolchain:t -> t

val equal : t -> t -> bool

val of_string_opt : string -> t option

val of_string : string -> t

val to_string : t -> string

val parse_string_exn : Loc.t * string -> t

include Dune_lang.Conv.S with type t := t

module Infix : Comparator.OPS with type t = t

val to_dyn : t -> Dyn.t

module Map : Map.S with type key = t

module Set : Set.S with type elt = t

module Top_closure :
  Top_closure.S with type key := t and type 'a monad := 'a Monad.Id.t
