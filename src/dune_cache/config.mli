open Stdune

(** While the main purpose of Dune cache is to speed up build times, it can also
    be used to check build reproducibility. When the check is enabled, Dune will
    re-execute randomly chosen build rules and compare their results with those
    stored in the cache. If the results differ, the rule is not reproducible and
    Dune will print out a corresponding warning. *)
module Reproducibility_check : sig
  type t =
    | Skip
    | Check_with_probability of float  (** [0 < p < 1] *)
    | Check

  (** Should we check the current build rule for reproducibility?

      - If [t = Skip], return [false].

      - If [t = Check_with_probability p], return [true] with probability [p].

      - If [t = Check], return [true]. *)
  val sample : t -> bool

  (** A helper function that returns the [Check_with_probability] variant only
      if the given probability is greater than zero and less than one. It also
      raises an error for values less than zero or greater than one. *)
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
