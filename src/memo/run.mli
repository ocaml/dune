open Stdune

(** A value of type [Run.t] represents a run of the build system and is used in
    the memoization framework as a dependency of functions that need to be
    recomputed on every build run.

    In the batch mode, there is only one run, so the [is_current t] predicate
    always returns [true]. In the file-watching mode, there may be multiple
    runs, separated by calls to [restart].

    Upon [restart], all previously created [Run.t] values stop being current, so
    the subsequent calls of [is_current] on these values will return [false]. *)
type t [@@immediate]

val to_dyn : t -> Dyn.t

(** Return the current run. *)
val current : unit -> t

(** Check whether this run is the current one. *)
val is_current : t -> bool

(** Compare runs: the current run is greater than earlier ones. *)
val compare : t -> t -> Ordering.t

(** End the current run and start a new one. *)
val restart : unit -> unit

(** A pair of [t]s representing the [(last_changed_at, last_validated_at)]
    timestamps of a memoized value, with the invariant
    [last_changed_at <= last_validated_at], packed into a single immediate.

    This is a memory-footprint optimization for cached values: a pair takes the
    same space as a single [t] instead of two. The packing assumes [t] values
    stay below [2 ^ 31 - 1], which is ample (one [restart] per second for ~68
    years). *)
module Pair : sig
  type run := t
  type t [@@immediate]

  val create : last_changed_at:run -> last_validated_at:run -> t
  val last_changed_at : t -> run
  val last_validated_at : t -> run

  (** [with_last_validated_at t ~last_validated_at] preserves [last_changed_at t].
      The caller must ensure [last_changed_at t <= last_validated_at] still holds. *)
  val with_last_validated_at : t -> last_validated_at:run -> t
end
