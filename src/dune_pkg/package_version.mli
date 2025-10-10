open! Stdune

type t = Dune_lang.Package_version.t

val of_string : string -> t
val of_string_user_error : Loc.t * string -> (t, User_message.t) result
val to_string : t -> string
val equal : t -> t -> bool
val compare : t -> t -> ordering
val hash : t -> int
val digest_feed : t Dune_digest.Feed.t
val to_dyn : t -> Dyn.t
val encode : t Dune_lang.Encoder.t
val decode : t Dune_lang.Decoder.t
val of_opam_package_version : OpamPackage.Version.t -> t
val to_opam_package_version : t -> OpamPackage.Version.t
val dev : t

include Comparable_intf.S with type key := t
