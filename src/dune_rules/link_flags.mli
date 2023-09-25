(** OCaml flags *)

open Import

type t

module Spec : sig
  type t

  val equal : t -> t -> bool
  val decode : check:unit Dune_lang.Decoder.t option -> t Dune_lang.Decoder.fields_parser
  val standard : t
end

val make
  :  spec:Spec.t
  -> default:t
  -> eval:
       (Ordered_set_lang.Unexpanded.t
        -> standard:string list Action_builder.t
        -> string list Action_builder.t)
  -> t

val default : default_cxx_link_flags:string list Action_builder.t -> t
val get : use_standard_cxx_flags:bool -> t -> string list Action_builder.t
val dump : t -> Dune_lang.t list Action_builder.t
