open Stdune

(** While the main purpose of Dune cache is to speed up build times, it can also
    be used to check build reproducibility. When the check is enabled, Dune will
    re-execute randomly chosen build rules and compare their results with those
    stored in the cache. If the results differ, the rule is not reproducible and
    Dune will print out a corresponding warning. *)
module Reproducibility_check : sig
  type t =
    | Skip
    | Check of { check_probability : float }

  (** If [t = Skip], this function always returns [false]. Otherwise, it returns
      [true] with the [check_probability]. *)
  val sample : t -> bool

  val to_dyn : t -> Dyn.t
end

(** All configuration settings of Dune's local and cloud (in future) caches. *)
type t =
  | Disabled
  | Enabled of
      { storage_mode : Dune_cache_storage.Mode.t
      ; reproducibility_check : Reproducibility_check.t
      }
