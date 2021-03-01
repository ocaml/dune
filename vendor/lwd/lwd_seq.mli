(** {1 Sequence manipulation}

    [Lwd_seq] is an ordered collection with a pure interface.
    Changes to collections are easy to track.

    A collection can be transformed with the usual map, filter and fold
    combinators. If the collection is updated, shared elements (in the sense of
    physical sharing), the result of the previous transformation will be reused
    for these elements.

    The book-keeping overhead is O(n) in the number of changes, so O(1) per
    element.
*)

type +'a t
type +'a seq = 'a t
(** The type of sequences *)

(** {2 Primitive constructors} *)

val empty : 'a seq
(** A sequence with no element. *)

val element : 'a -> 'a seq
(** A singleton sequence. The physical identity of the element is considered
    when reusing previous computations.

    If you do:

    {[let x1 = element x
      let x2 = element x]}

    Then [x1] and [x2] are seen as different elements and no sharing will be
    done during transformation.
*)

val concat : 'a seq -> 'a seq -> 'a seq
(** Concatenate two sequences into a bigger one.
    As for [element], the physical identity of a sequence is considered for
    reuse.
*)

(** {2 Looking at sequence contents} *)

type ('a, 'b) view =
  | Empty
  | Element of 'a
  | Concat of 'b * 'b

val view : 'a seq -> ('a, 'a seq) view
(** View how a sequence is defined *)

(** {2 Conversion between sequences, lists and arrays} *)

val transform_list : 'a list -> ('a -> 'b seq) -> 'b seq
(** Produce a sequence by transforming each element of a list and concatenating
    all results. *)

val transform_array : 'a array -> ('a -> 'b seq) -> 'b seq
(** Produce a sequence by transforming each element of an array and
    concatenating all results. *)

val of_list : 'a list -> 'a seq
(** Produce a sequence from a list *)

val of_array : 'a array -> 'a seq
(** Produce a sequence from an array *)

val to_list : 'a seq -> 'a list
(** Produce a list from a sequence *)

val to_array : 'a seq -> 'a array
(** Produce an array from a sequence *)

(** {2 Balanced variant of sequences} *)

module Balanced : sig

  (** A variant of the sequence type that guarantees that the depth of a
      transformation, measured as the number of nested [concat] nodes, grows in
      O(log n) where n is the number of elements in the sequnce.

     This is useful to prevent stack overflows and to avoid degenerate cases
     where a single element changes, but it is at the end of a linear sequence
     of [concat] nodes, thus making the total work O(n).
     For instance, in:

      {[concat e1 (concat e2 (concat e3 (... (concat e_n))...))]}

     If [e_n] changes, the whole spine has to be recomputed.

     Using [Balanced.concat], the representation will be re-balanced
     internally. Then [Balanced.view] should be used to access the balanced
     sequence.

     When working with balanced sequences in a transformation pipeline, it is
     only useful to balance the first sequence of the pipeline. Derived
     sequence will have a depth bounded by the depth of the first one.
  *)

  type 'a t = private 'a seq
  (** Type of balanced sequences *)

  val empty : 'a t
  val element : 'a -> 'a t
  val concat : 'a t -> 'a t -> 'a t

  val view : 'a t -> ('a, 'a t) view
end

(** {2 Transforming sequences} *)

(**
   All sequences live in [Lwd] monad: if a sequence changes slightly, parts
   that have not changed will not be re-transformed.
*)

val fold :
  map:('a -> 'b) -> reduce:('b -> 'b -> 'b) -> 'a seq Lwd.t -> 'b option Lwd.t
(** [fold ~map ~reduce] transforms a sequence.
    If the sequence is non-empty, the [map] function is applied to element
    nodes and the [reduce] function is used to combine transformed concatenated
    nodes.
    If the sequence is empty, None is returned.
*)

val fold_monoid :
  ('a -> 'b) -> 'b Lwd_utils.monoid -> 'a seq Lwd.t -> 'b Lwd.t
(** Like [fold], but reduction and default value are defined by a [monoid] *)

val map :
  ('a -> 'b) -> 'a seq Lwd.t -> 'b seq Lwd.t
(** [map f] transforms a sequence by applying [f] to each element. *)

val filter :
  ('a -> bool) -> 'a seq Lwd.t -> 'a seq Lwd.t
(** [filter p] transforms a sequence by keeping elements that satisfies [p]. *)

val filter_map :
  ('a -> 'b option) -> 'a seq Lwd.t -> 'b seq Lwd.t
(** Filter and map elements at the same time *)

val lift : 'a Lwd.t seq Lwd.t -> 'a seq Lwd.t
(** Remove a layer of [Lwd] inside a sequence. *)

val bind : 'a seq Lwd.t -> ('a -> 'b seq Lwd.t) -> 'b seq Lwd.t
(** Sequence forms a monad too... *)

val seq_bind : 'a seq Lwd.t -> ('a -> 'b seq) -> 'b seq Lwd.t
(** Sequence forms a monad too... *)

val monoid : 'a t Lwd_utils.monoid
(** Monoid instance for sequences *)

val lwd_monoid : 'a t Lwd.t Lwd_utils.monoid
(** Monoid instance for reactive sequences *)

(** {2 Low-level interface for observing changes} *)

module Reducer : sig
  (* The interface allows to implement incremental sequence transformation
     outside of the [Lwd] monad.
     Actually, the Lwd functions above are implemented on top of this
     interface.
  *)

  (* A [('a, 'b) reducer] value stores the state necessary to incrementally
     transform an ['a seq] to ['b].
     In essence, the Lwd functions just hide a reducer value.
  *)
  type ('a, 'b) reducer

  (* A new reducer that transforms sequences with the given [map] and [reduce]
     functions.  The reducer starts from the [empty] sequence.  *)
  val make : map:('a -> 'b) -> reduce:('b -> 'b -> 'b) -> ('a, 'b) reducer

  (* Updates the [reducer] to transform another sequence.
     Intermediate nodes are reused when possible.
     Only the "reuse plan" is computed by [update], actual transformation is
     done by the [reduce] function.
   *)
  val update : ('a, 'b) reducer -> 'a seq -> ('a, 'b) reducer

  (* Returns the reduced ['b] value if the sequence is non-empty or [None] if
     the sequence is empty.
     Because transformation is done lazily, [reduce] is the only function
     that can call [map] and [reduce].
   *)
  val reduce : ('a, 'b) reducer -> 'b option

  (* Sometimes it is important to track the elements that disappeared from a
     sequence. The ['b dropped] type represent all the intermediate result that
     were referenced by a reducer and are no longer after an update.
  *)
  type 'b dropped
  val update_and_get_dropped :
    ('a, 'b) reducer -> 'a seq -> 'b dropped * ('a, 'b) reducer

  val fold_dropped :
    [<`All|`Map|`Reduce] -> ('a -> 'b -> 'b) -> 'a dropped -> 'b -> 'b
end

