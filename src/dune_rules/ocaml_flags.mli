(** OCaml flags *)
open! Dune_engine

open! Stdune

type t

module Spec : sig
  type t

  val equal : t -> t -> bool

  val decode : t Dune_lang.Decoder.fields_parser

  val standard : t
end

val make :
     spec:Spec.t
  -> default:t
  -> eval:
       (   Ordered_set_lang.Unexpanded.t
        -> standard:string list Build.t
        -> string list Build.t)
  -> t

val default : dune_version:Dune_lang.Syntax.Version.t -> profile:Profile.t -> t

val empty : t

val of_list : string list -> t

val get : t -> Mode.t -> string list Build.t

val append_common : t -> string list -> t

val prepend_common : string list -> t -> t

val with_vendored_warnings : t -> t

val common : t -> string list Build.t

val dump : t -> Dune_lang.t list Build.t
