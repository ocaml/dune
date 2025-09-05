open Import

module Local : sig
  type t := Path.t

  val encode : dir:Path.t -> t Encoder.t
  val decode : dir:Path.t -> t Decoder.t
end
