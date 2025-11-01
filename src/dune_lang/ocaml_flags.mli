(** OCaml flags *)

open Import

type 'a t =
  { common : 'a
  ; specific : 'a Lib_mode.Map.t
  }

module Spec : sig
  type nonrec t = Ordered_set_lang.Unexpanded.t t

  val equal : t -> t -> bool
  val decode : t Decoder.fields_parser
  val standard : t

  val make
    :  common:Ordered_set_lang.Unexpanded.t
    -> specific:Ordered_set_lang.Unexpanded.t Lib_mode.Map.t
    -> t
end

val open_flags : Module_name.t list -> string list
