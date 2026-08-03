(** A series-parallel graph representing the dependencies of a Memo node.

    Dependencies discovered sequentially (via [let*]/[>>=]) form a sequence; dependencies
    discovered through a parallel combinator (e.g. [fork_and_join]) form a parallel
    section. This lets [changed_or_not] check independent dependencies concurrently while
    still checking sequential ones in order. *)

open! Stdune

type 'node t

val empty : 'node t

(** Like [t] but supports cheap appending of new dependencies. *)
module Dynamic : sig
  type 'node static := 'node t
  type 'node t

  val empty : 'node t

  (** Append a dependency discovered after the existing ones. *)
  val append_seq : 'node t -> node:'node -> 'node t

  (** Append a parallel section of [num_threads] threads, where thread [i]'s dependencies
      are [f i], each captured independently. Builds the section directly from [f], avoiding
      an intermediate list of the threads' dependencies. *)
  val append_par_init : 'node t -> num_threads:int -> f:(int -> 'node static) -> 'node t

  val to_static : 'node t -> 'node static
end

(** [changed_or_not t ~f] checks each dependency with [f]. Sequential sections are checked
    in order and the check stops early at the first [Changed]/[Cancelled]; parallel
    sections are checked concurrently and their results combined.

    [ok_to_recompute_eagerly] is [true] when the dependency is a direct child of a parallel
    section, telling [f] that it may eagerly recompute a dependency without a cutoff in
    parallel with its siblings (instead of deferring the recomputation). *)
val changed_or_not
  :  'node t
  -> f:(ok_to_recompute_eagerly:bool -> 'node -> 'cycle Changed_or_not.t Fiber.t)
  -> 'cycle Changed_or_not.t Fiber.t

module For_debugging : sig
  val to_list : 'node t -> 'node list

  (** A structural rendering that preserves the [Seq]/[Par]/[Singleton]/[Empty]
      nesting (and intra-[Seq] order), unlike [to_list] which flattens. *)
  val to_dyn : ('node -> Dyn.t) -> 'node t -> Dyn.t
end
