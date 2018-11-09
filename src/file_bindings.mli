open Stdune

type 'a file =
  { src : 'a
  ; dst : 'a option
  }

type 'a t = 'a file list

val map : 'a t -> f:('a -> 'b) -> 'b t

val empty : 'a t

val path_map : string t -> dir:Path.t -> Path.t String.Map.t

module Unexpanded : sig
  type nonrec t = String_with_vars.t t
  val decode : t Stanza.Decoder.t
end
