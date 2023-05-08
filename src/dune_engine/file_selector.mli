(** A [File_selector.t] is a predicate evaluated on a set of file names in a
    specified directory. *)

open Import

(** TODO get rid of this *)
module Predicate_with_id : sig
  (** Predicates are functions from 'a -> bool along with a uniquely identifying
      string. The uniquely identifying string allows us to safely memoize on the
      predicate *)

  type 'a t

  val equal : 'a t -> 'a t -> bool

  (**[create id ~f] creates a predicate defined by [f] identified uniquely with
     [id]. [id] is used to safely compare predicates for equality for
     memoization *)
  val create : id:Dyn.t Lazy.t -> f:('a -> bool) -> 'a t

  (** The predicate that evaluates to [true] for any query. *)
  val true_ : _ t

  (** The predicate that evaluates to [false] for any query. *)
  val false_ : _ t
end

type t

val dir : t -> Path.t

val predicate : t -> Filename.t Predicate_with_id.t

val only_generated_files : t -> bool

val of_glob : dir:Path.t -> Glob.t -> t

val create :
     dir:Path.t
  -> ?only_generated_files:bool
  -> Filename.t Predicate_with_id.t
  -> t

val equal : t -> t -> bool

val hash : t -> int

val compare : t -> t -> Ordering.t

val encode : t Dune_sexp.Encoder.t

(** [to_dyn] is used as a marshallable representation of [t] (to compute
    digests), so it must be injective *)
val to_dyn : t -> Dyn.t

val test : t -> Path.t -> bool
