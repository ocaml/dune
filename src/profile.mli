(** Defines build profile for dune. Only one profile is active per context. Some
    profiles are treat specially by dune. *)
open Stdune

type t =
  | Dev
  | Release
  | User_defined of string

val is_dev : t -> bool

val is_release : t -> bool

val to_string : t -> string

val of_string : string -> t (* TODO add error handling *)

val default : t

val decode : t Dune_lang.Decoder.t

val to_dyn : t -> Dyn.t
