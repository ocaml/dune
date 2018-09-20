open Stdune

include Dune_lang.Conv with type t = Path.t

module Local : sig
  val encode : dir:Path.t -> Path.t Dune_lang.Encoder.t

  val decode : dir:Path.t -> Path.t Dune_lang.Decoder.t
end
