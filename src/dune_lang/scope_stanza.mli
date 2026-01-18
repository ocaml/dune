open Import

type t

val decode : dir:(Loc.t * Filename.t) Decoder.t -> t Decoder.t
val directory : t -> Filename.t
val loc : t -> Loc.t
val eval_packages : t -> standard:Package_name.t list -> Package_name.Set.t
