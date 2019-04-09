open Stdune

type 'a t =
  { src : 'a
  ; dst : 'a option
  }

val dst_path : string t -> dir:Path.t -> Path.t
val src_path : string t -> dir:Path.t -> Path.t

val map : 'a t -> f:('a -> 'b) -> 'b t

val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t

module L : sig
  type nonrec 'a t = 'a t list

  val empty : 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val is_empty : _ t -> bool

  module Unexpanded : sig
    type nonrec t = String_with_vars.t t
    val decode : t Stanza.Decoder.t
  end
end
