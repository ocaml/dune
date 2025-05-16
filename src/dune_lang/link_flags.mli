open Import

type 'a t =
  { link_flags : 'a
  ; link_flags_cxx : 'a
  }

module Spec : sig
  type nonrec t = Ordered_set_lang.Unexpanded.t t

  val standard : t
  val decode : check:unit Decoder.t option -> (t, Decoder.fields) Decoder.parser
  val equal : t -> t -> bool
end

val get : use_standard_cxx_flags:bool -> 'a t -> 'a
