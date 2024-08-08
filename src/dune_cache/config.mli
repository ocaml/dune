open Stdune

(** While the main purpose of Dune cache is to speed up build times, it can also
    be used to check build reproducibility. When the check is enabled, Dune will
    re-execute randomly chosen build rules and compare their results with those
    stored in the cache. If the results differ, the rule is not reproducible and
    Dune will print out a corresponding warning. *)
module Reproducibility_check : sig
  (** We treat the probabilities 0 and 1 specially because in these two extremes
      the behaviour of Dune can be completely deterministic. In particular, the
      [sample] function doesn't require any randomness in the [Skip] and [Check]
      cases. This speeds up sampling and also makes it clear that in these cases
      Dune's behaviour is indeed deterministic. *)
  type t =
    | Skip
    | Check_with_probability of float (** [0 < p < 1] *)
    | Check

  (** Should we check the current build rule for reproducibility?

      - If [t = Skip], return [false].

      - If [t = Check_with_probability p], return [true] with probability [p].

      - If [t = Check], return [true]. *)
  val sample : t -> bool

  (** A helper function that turns a [p : float] into [t].

      - If [p = 0], return [Skip].

      - If [0 < p < 1], return [Check_with_probability p].

      - If [p = 1], return [Check].

      - If [p < 0] or [p > 1], raise a user error with a specified location. We
        consider it a user error because [p] can come from the configuration
        file, environment variable or command line. *)
  val check_with_probability : ?loc:Loc.t -> float -> t

  val to_dyn : t -> Dyn.t
end

(** All configuration settings of Dune's local and cloud (in future) caches. *)
type t =
  | Disabled
  | Enabled of
      { storage_mode : Dune_cache_storage.Mode.t
      ; reproducibility_check : Reproducibility_check.t
      }
