(** Landlock filesystem policies. *)

val available : unit -> bool

module Policy : sig
  (** A write policy satisfies these invariants:

      - there is at least one denied path;
      - all paths existed and were resolved with [realpath] when the policy was
        created;
      - every allowed path is a strict descendant of at least one denied path;
      - no allowed path is equal to or an ancestor of a denied path.

      Paths are not deduplicated. *)
  type t

  val create : deny_write:Path.External.t list -> allow_write:Path.External.t list -> t
end

module Ruleset : sig
  type t

  val create : Policy.t -> t
  val file_descr : t -> Unix.file_descr
  val close : t -> unit

  (** Restrict the calling thread with [t]. The restriction is inherited by
      subsequently created children and cannot be removed. *)
  val restrict_self : t -> unit
end

(** Create and immediately enforce [policy] on the calling thread. *)
val restrict_self : Policy.t -> unit
