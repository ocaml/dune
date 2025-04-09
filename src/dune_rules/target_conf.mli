[@@@warning "-33"]

open Stdune
open Dune_lang
module Name = String

type t = String_with_vars.t Targets_spec.Named_target.t Bindings.t

val decode : t Dune_lang.Decoder.t
val empty : t
