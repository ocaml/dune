open Import

type fetch =
  { url : Loc.t * OpamUrl.t
  ; checksum : (Loc.t * Checksum.t) option
  }

type t =
  | External_copy of Loc.t * Path.External.t
  | Fetch of fetch

val equal : t -> t -> bool
val decode : (Path.External.t -> t) Dune_sexp.Decoder.t
val encode : t -> Dune_sexp.t
val to_dyn : t -> Dyn.t
val remove_locs : t -> t
val compute_missing_checksum : t -> Package_name.t -> t Fiber.t
