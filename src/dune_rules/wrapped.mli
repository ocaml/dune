open! Dune_engine
open Stdune

type t =
  | Simple of bool
  | Yes_with_transition of string

include Dune_lang.Conv.S with type t := t

val to_bool : t -> bool

val to_dyn : t -> Dyn.t
