(** Encoding + Decoding *)

module type S = sig
  type t

  val decode : t Decoder.t
  val encode : t Encoder.t
end
