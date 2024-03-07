type +'a t
(** A dynamic document of type ['a]. Documents can be produced in several
    different ways:

    - operators, such as {!map}, {!bind}, {!app}, {!pair}, etc.
      combine several documents into one. The result is (lazily)
      updated whenever the sub-documents are.

    - variables {!var}, a mutable reference.
    - primitive documents {!prim}, providing custom leaves to trees of
      documents.
*)

val return : 'a -> 'a t
(** The content document with the given value inside *)

val pure : 'a -> 'a t
(** Alias to {!return} *)

val map : 'a t -> f:('a -> 'b) -> 'b t
(** [map d ~f] is the document that has value [f x] whenever [d] has value [x] *)

val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
(** [map2 d1 d2 ~f] is the document that has value [f x1 x2] whenever
    [d1] has value [x1] and [d2] has value [x2] *)

val join : 'a t t -> 'a t
(** Monadic operator [join d] is the document pointed to by document [d].
    This is powerful but potentially costly in case of recomputation.
*)

val bind : 'a t -> f:('a -> 'b t) -> 'b t
(** Monadic bind, a mix of {!join} and {!map} *)

val app : ('a -> 'b) t -> 'a t -> 'b t
(** Applicative: [app df dx] is the document that has value [f x]
    whenever [df] has value [f] and [dx] has value [x] *)

val pair : 'a t -> 'b t -> ('a * 'b) t
(** [pair a b] is [map2 (fun x y->x,y) a b] *)

val is_pure : 'a t -> 'a option
(** [is_pure x] will return [Some v] if [x] was built with [pure v] or
    [return v].

    Normal code should not rely on the "reactive-ness" of a value, but this is
    often useful for optimising reactive data structures.
*)

type 'a var
(** The workhorse of Lwd: a mutable variable that also tracks dependencies.
    Every time {!set} is called, all documents that depend on this variable
    via {!map}, {!bind}, etc. will be at least partially invalidated
    and will be recomputed incrementally on demand. *)

val var : 'a -> 'a var
(** Create a new variable with the given initial value *)

val get : 'a var -> 'a t
(** A document that reflects the current content of a variable *)

val set : 'a var -> 'a -> unit
(** Change the variable's content, invalidating all documents depending
    on it. *)

val peek : 'a var -> 'a
(** Observe the current value of the variable, without any dependency
    tracking. *)

type +'a prim
(** A primitive document. It can correspond, for example, to
    a primitive UI element.

    A primitive is a resource with [acquire] and [release] functions
    to manage its lifecycle. *)

val prim : acquire:('a prim -> 'a) -> release:('a prim -> 'a -> unit) -> 'a prim
(** create a new primitive document.
    @param acquire is called when the document becomes observed (indirectly)
    via at least one {!root}.  The resulting primitive is passed as an argument
    to support certain recursive use cases.
    @param release is called when the document is no longer observed.
    Internal resources can be freed. *)

val get_prim : 'a prim -> 'a t
val invalidate : 'a prim -> unit

(** Some document might change variables during their evaluation.
    These are called "unstable" documents.

    Evaluating these might need many passes to eventually converge to a value.
    The `fix` operator tries to stabilize a sub-document by repeating
    evaluation until a stable condition is reached.
*)
val fix : 'a t -> wrt:_ t -> 'a t

val default_unsafe_mutation_logger : unit -> unit
val unsafe_mutation_logger : (unit -> unit) ref

(** Releasing unused graphs *)
type release_failure = exn * Printexc.raw_backtrace

exception Release_failure of exn option * release_failure list

type release_queue
val make_release_queue : unit -> release_queue
val flush_release_queue : release_queue -> release_failure list

type +'a root
(** A root of computation, whose value(s) over time we're interested in. *)

val observe : ?on_invalidate:('a -> unit) -> 'a t -> 'a root
(** [observe x] creates a root that contains document [x].
    @param on_invalidate is called whenever the root is invalidated
    because the content of [x] has changed. This can be useful to
    perform side-effects such as re-rendering some UI. *)

val set_on_invalidate : 'a root -> ('a -> unit) -> unit
(** Change the callback for the root.
    See [observe] for more details. *)

val sample : release_queue -> 'a root -> 'a
(** Force the computation of the value for this root.
    The value is cached, so this is idempotent, until the next invalidation. *)

val is_damaged : 'a root -> bool
(** [is_damaged root] is true if the root doesn't have a valid value in
    cache. This can be the case if the value was never computed, or
    if it was computed and then invalidated. *)

val release : release_queue -> 'a root -> unit
(** Forget about this root and release sub-values no longer reachable from
    any root. *)

val quick_sample : 'a root -> 'a

val quick_release : 'a root -> unit

module Infix : sig
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
end

(* For debug purposes *)
val dump_trace : 'a t -> unit
