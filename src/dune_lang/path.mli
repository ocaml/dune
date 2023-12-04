open Stdune

module Local : sig
  type t := Path.t

  val encode : dir:Path.t -> t Dune_sexp.Encoder.t
  val decode : dir:Path.t -> t Dune_sexp.Decoder.t
end
