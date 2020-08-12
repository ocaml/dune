(** Linking modes for binaries *)

open! Stdune

type t =
  | C
  | Exe
  | Object
  | Shared_object
  | Plugin
  | Js

include Dune_lang.Conv.S with type t := t

val all : t list

val to_dyn : t -> Dyn.t
