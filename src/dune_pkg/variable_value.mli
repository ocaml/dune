open! Import

type t

val true_ : t
val false_ : t

(** Construct a value of type string *)
val string : string -> t

val equal : t -> t -> bool
val to_dyn : t -> Dyn.t
val decode : t Decoder.t
val encode : t Encoder.t
val to_string : t -> string
val to_opam_filter : t -> OpamTypes.filter
val to_opam_variable_contents : t -> OpamTypes.variable_contents
