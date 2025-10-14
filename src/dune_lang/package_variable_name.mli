open Import

type t

val to_opam : t -> OpamVariable.t
val of_opam : OpamVariable.t -> t
val compare : t -> t -> Ordering.t
val to_dyn : t -> Dyn.t
val encode : t Encoder.t
val decode : t Decoder.t
val equal : t -> t -> bool

module Map : Map.S with type key = t
module Set : Set.S with type elt = t and type 'a map = 'a Map.t

val of_string : string -> t
val to_string : t -> string
val hash : t -> int
val digest_feed : t Dune_digest.Feed.t
val arch : t
val os : t
val os_version : t
val os_distribution : t
val os_family : t
val opam_version : t
val with_test : t
val with_doc : t
val with_dev_setup : t
val sys_ocaml_version : t
val name : t
val version : t
val post : t
val build : t
val dev : t
val one_of : t -> t list -> bool

(** The set of variable names whose values are expected to differ depending on
    the current platform. *)
val platform_specific : Set.t

module Project : sig
  val encode : t Encoder.t
  val decode : t Decoder.t
end
