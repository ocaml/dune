open Import

type t =
  { url : Loc.t * OpamUrl.t
  ; checksum : (Loc.t * Checksum.t) option
  }

val equal : t -> t -> bool
val decode : (Path.External.t -> t) Dune_sexp.Decoder.t
val encode : t -> Dune_sexp.t
val to_dyn : t -> Dyn.t
val remove_locs : t -> t
val compute_missing_checksum : t -> Package_name.t -> pinned:bool -> t Fiber.t
val external_copy : Loc.t * Path.External.t -> t
val kind : t -> [ `Directory_or_archive of Path.External.t | `Fetch ]
