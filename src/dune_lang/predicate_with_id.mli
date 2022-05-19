(** Predicates are functions from 'a -> bool along with a uniquely identifying
    string. The uniquely identifying string allows us to safely memoize on the
    predicate *)

(* TODO move back to dune_engine? this is really only needed for
   [File_selector.t] *)

open Stdune

type 'a t

val predicate : 'a t -> 'a Predicate.t

val equal : 'a t -> 'a t -> bool

val compare : 'a t -> 'a t -> Ordering.t

val hash : _ t -> int

val encode : _ t Encoder.t

val to_dyn : _ t -> Dyn.t

(**[create id ~f] creates a predicate defined by [f] identified uniquely with
   [id]. [id] is used to safely compare predicates for equality for memoization *)
val create : id:Dyn.t Lazy.t -> f:('a -> bool) -> 'a t

(** The predicate that evaluates to [true] for any query. *)
val true_ : _ t

(** The predicate that evaluates to [false] for any query. *)
val false_ : _ t

val test : 'a t -> 'a -> bool

(** the user of this function must take care not to break the uniqueness of the
    underlying representation *)
val contramap : 'a t -> f:('b -> 'a) -> map_id:(Dyn.t -> Dyn.t) -> 'b t
