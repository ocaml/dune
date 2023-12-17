open Import

type 'a t =
  { flags : 'a
  ; explain : Blang.t option
  }

val map : f:('a -> 'b) -> 'a t -> 'b t
val equal : Ordered_set_lang.Unexpanded.t t -> Ordered_set_lang.Unexpanded.t t -> bool
val decode : Ordered_set_lang.Unexpanded.t t Dune_lang.Decoder.t
val empty : Ordered_set_lang.Unexpanded.t t
val default : string list t
val dump : string list Action_builder.t t -> Dune_lang.t list Action_builder.t
