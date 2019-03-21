(** Predicates are functions from 'a -> bool along with a uniquely identifying
    string. The uniquely identifying string allows us to safely memoize on the
    predicate *)

open Stdune

type 'a t

val equal : 'a t -> 'a t -> bool

val hash : _ t -> int

val to_sexp : _ t Sexp.Encoder.t

val to_dyn : _ t -> Dyn.t

(**[create id ~f] creates a predicate defined by [f] identified uniquely with
   [id]. [id] is used to safely compare predicates for equality for
   memoization *)
val create : id:Sexp.t Lazy.t -> f:('a -> bool) -> 'a t

val test : 'a t -> 'a -> bool

(** the user of this function must take care not to break the uniqueness of the
    underlying representation *)
val contramap : 'a t -> f:('b -> 'a) -> map_id:(Sexp.t -> Sexp.t) -> 'b t
