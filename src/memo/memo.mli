open! Stdune

(** A type of memoized computations that can be recomputed incrementally when
    their dependencies change. *)
type 'a t

type 'a memo := 'a t

(** A type of Memo-like monads. *)
module type S = sig
  include Monad.S
  module List : Monad.List with type 'a t := 'a t

  (** Inject a value of type ['a Memo.t] into ['a t]. *)
  val of_memo : 'a memo -> 'a t
end

include S with type 'a t := 'a t
module Option : Monad.Option with type 'a t := 'a t
module Result : Monad.Result with type 'a t := 'a t

(* CR-someday amokhov: Return the set of exceptions explicitly. *)
val run : 'a t -> 'a Fiber.t

(** Every error gets reported twice: once early, in non-deterministic order, by
    calling [handler_error], and once later, in deterministic order, by raising
    a fiber exception.

    [handle_error_no_raise] must not raise exceptions, otherwise internal memo
    invariants get messed up and you get confusing errors like "Attempted to
    create a cached value based on some stale inputs".

    Nested calls of [run_with_error_handler] are not allowed.

    If multiple calls to [run_with_error_handler] happen concurrently (possibly
    with different error handlers), then each handler will correctly receive all
    errors it would be expected to receive if run independently. However, each
    error will only be sent "early" to one of the handlers, while the other
    handler will get this error delayed. If this limitation becomes problematic,
    it may be possible to lift it by eagerly bubbling up each error through
    individual dependency edges instead of sending errors directly to the
    handler in scope. *)
val run_with_error_handler
  :  (unit -> 'a t)
  -> handle_error_no_raise:(Exn_with_backtrace.t -> unit Fiber.t)
  -> 'a Fiber.t

(** [of_reproducible_fiber fiber] injects a fiber into the build monad. The
    given fiber must be "reproducible", i.e. executing it multiple times should
    always yield the same result. It is up to the caller to ensure that this
    property holds. If it doesn't, use [of_non_reproducible_fiber]. *)
val of_reproducible_fiber : 'a Fiber.t -> 'a t

(** [of_non_reproducible_fiber fiber] injects a fiber into the build monad. The
    fiber is considered to be "non-reproducible", i.e. it may return different
    values each time it is executed (for example, the current time), and it will
    therefore be re-executed on every build run. *)
val of_non_reproducible_fiber : 'a Fiber.t -> 'a t

(** Convert a thunk to a Memo computation, making sure the thunk runs in the context of
    the Memo computation rather than in the current context.

    [of_thunk f] is equivalent to [return () >> f] but is more explicit. *)
val of_thunk : (unit -> 'a t) -> 'a t

(** Like [of_thunk] but accepts functions of any argument. *)
val of_thunk_apply : ('a -> 'b t) -> 'a -> 'b t

(** Combine results of two computations executed in sequence. *)
val both : 'a t -> 'b t -> ('a * 'b) t

(** Combine results of two computations executed in parallel. Note that this
    function combines both successes and errors: if one of the computations
    fails, we let the other one run to completion, to give it a chance to raise
    its errors too. Regardless of the outcome (success or failure), the result
    will collect dependencies from both of the computations. All other parallel
    execution combinators have the same error/dependencies semantics. *)
val fork_and_join : (unit -> 'a t) -> (unit -> 'b t) -> ('a * 'b) t

val fork_and_join_unit : (unit -> unit t) -> (unit -> 'a t) -> 'a t

(** This uses a sequential implementation. We use the short name to conform with
    the [Applicative] interface. See [all_concurrently] for the version with
    concurrency. *)
val all : 'a t list -> 'a list t

val all_concurrently : 'a t list -> 'a list t
val when_ : bool -> (unit -> unit t) -> unit t
val sequential_map : 'a list -> f:('a -> 'b t) -> 'b list t
val sequential_iter : 'a list -> f:('a -> unit t) -> unit t
val parallel_map : 'a list -> f:('a -> 'b t) -> 'b list t
val parallel_iter : 'a list -> f:('a -> unit t) -> unit t
val parallel_iter_seq : 'a Seq.t -> f:('a -> unit t) -> unit t

module Make_parallel_map (Map : Map.S) : sig
  val parallel_map : 'a Map.t -> f:(Map.key -> 'a -> 'b t) -> 'b Map.t t
end

(** A table memoizing results of executing a function. *)
module Table : sig
  type ('input, 'output) t
end

(** A stack frame within a computation. *)
module Stack_frame : sig
  type t

  val to_dyn : t -> Dyn.t
  val name : t -> string option
  val input : t -> Dyn.t

  (** Checks if the stack frame is a frame of the given memoized function and if
      so, returns [Some i] where [i] is the argument of the function. *)
  val as_instance_of : t -> of_:('input, _) Table.t -> 'input option

  val human_readable_description : t -> User_message.Style.t Pp.t option
end

(** Errors raised by user-supplied memoized functions that have been augmented
    with Memo call stack information. *)
module Error : sig
  type t

  exception E of t

  (** Get the underlying exception.*)
  val get : t -> exn

  (** Return the stack leading to the node which raised the exception.*)
  val stack : t -> Stack_frame.t list
end

module Cycle_error : sig
  type t

  exception E of t

  (** Get the list of stack frames in this cycle. *)
  val get : t -> Stack_frame.t list
end

(* CR-someday amokhov: The current implementation memoizes all errors that are
   not wrapped into [Non_reproducible], which may be inconvenient in some cases,
   e.g. if a build action fails due to a spurious error, such as running out of
   memory. Right now, the only way to force such actions to be rebuilt is to
   restart Dune, which clears all memoized errors. In future, we would like to
   provide a way to rerun all failed actions without restarting the build, e.g.
   via a Dune RPC call. *)

(** Mark an exception as non-reproducible to indicate that it shouldn't be
    cached. *)
exception Non_reproducible of exn

(** [Invalidation] describes a set of nodes to be invalidated between
    memoization runs. These sets can be combined into larger sets to then be
    passed to [reset]. *)
module Invalidation : sig
  type t

  include Monoid.S with type t := t

  (** Reasons for invalidating a node or cache, currently used only to inform
      the user why Dune is restarting, see [reasons_hum].

      - [Unknown]: This should only be used in situations where the reason is
        provided by other parts of the invalidation data structure. We should
        strive to be able to always explain the user the reason for a restart.

      - [Path_changed]: Dune restarted because a path it watched changed.

      - [Event_queue_overflow]: Dune file watcher's queue overflow, which
        requires a full rebuild.

      - [Upgrade]: Dune upgrader initiated a full rebuild.

      - [Test]: Use this reason in testsuites. *)
  module Reason : sig
    type t =
      | Unknown
      | Path_changed of Path.t
      | Event_queue_overflow
      | Upgrade
      | Test
      | Variable_changed of string
  end

  val is_empty : t -> bool

  (** Clear all memoization tables. We use it if the incremental mode is not
      enabled. *)
  val clear_caches : reason:Reason.t -> t

  (** Invalidate all computations stored in a given [memo] table. *)
  val invalidate_cache : reason:Reason.t -> _ Table.t -> t

  (** A list of human-readable strings explaining the reasons for invalidation.
      The list is truncated to [max_elements] elements, with [max_elements = 1]
      by default. Raises if [max_elements <= 0]. *)
  val details_hum : ?max_elements:int -> t -> string list
end

(** Notify the memoization system that the build system has restarted. This
    removes the values specified by [Invalidation.t] from the memoization cache,
    and advances the current run. *)
val reset : Invalidation.t -> unit

module type Input = sig
  type t

  include Stdune.Table.Key with type t := t
end

(** [create name ~input ?cutoff f] creates a memoized version of the function
    [f : 'i -> 'o t]. The result of [f] for a given input is cached, so that the
    second time [exec t x] is called, the previous result is reused if possible.

    [exec t x] tracks what calls to other memoized functions [f x] performs.
    When the result of any of such dependent calls changes, [exec t x] will
    automatically recompute [f x].

    If the caller provides the [cutoff] equality check, we will use it to check
    if the function's output is the same as cached in the previous computation.
    If it's the same, we will be able to skip recomputing the functions that
    depend on it.

    If [human_readable_description] is passed, it will be used when displaying
    the Memo stack to the user.

    Running the computation may raise [Memo.Cycle_error.E] if a dependency cycle
    is detected. *)
val create
  :  string
  -> input:(module Input with type t = 'i)
  -> ?cutoff:('o -> 'o -> bool)
  -> ?human_readable_description:('i -> User_message.Style.t Pp.t)
  -> ('i -> 'o t)
  -> ('i, 'o) Table.t

module Store : sig
  module type Input = sig
    type t

    val to_dyn : t -> Dyn.t
  end

  module type S = Store_intf.S
end

(** Like [create] but accepts a custom [store] for memoization. This is useful
    when there is a custom data structure indexed by keys of type ['i] that is
    more efficient than the one that Memo uses by default (a plain hash table). *)
val create_with_store
  :  string
  -> store:(module Store.S with type key = 'i)
  -> input:(module Store.Input with type t = 'i)
  -> ?cutoff:('o -> 'o -> bool)
  -> ?human_readable_description:('i -> User_message.Style.t Pp.t)
  -> ('i -> 'o t)
  -> ('i, 'o) Table.t

(** Execute a memoized function. *)
val exec : ('i, 'o) Table.t -> 'i -> 'o t

(** Print the memoized call stack during execution. This is useful for debugging
    purposes. *)
val dump_stack : unit -> unit Fiber.t

val pp_stack : unit -> _ Pp.t Fiber.t

(** Get the memoized call stack during the execution of a memoized function. *)
val get_call_stack : unit -> Stack_frame.t list t

(** Insert a stack frame to make call stacks more precise when showing them to
    the user. *)
val push_stack_frame
  :  human_readable_description:(unit -> User_message.Style.t Pp.t)
  -> (unit -> 'a t)
  -> 'a t

(** A single build run. *)
module Run : sig
  type t

  module For_tests : sig
    val compare : t -> t -> Ordering.t
    val current : unit -> t
  end
end

(** Introduces a dependency on the current build run. *)
val current_run : unit -> Run.t t

module Cell : sig
  type ('i, 'o) t

  val input : ('i, _) t -> 'i
  val read : (_, 'o) t -> 'o memo

  (** Mark this cell as invalid, forcing recomputation of this value. The
      consumers may be recomputed or not, depending on early cutoff. *)
  val invalidate : reason:Invalidation.Reason.t -> _ t -> Invalidation.t
end

(** Create a "memoization cell" that focuses on a single input/output pair of a
    memoized function. *)
val cell : ('i, 'o) Table.t -> 'i -> ('i, 'o) Cell.t

val lazy_cell
  :  ?cutoff:('a -> 'a -> bool)
  -> ?name:string
  -> ?human_readable_description:(unit -> User_message.Style.t Pp.t)
  -> (unit -> 'a t)
  -> (unit, 'a) Cell.t

(** Returns the cached dependency graph discoverable from the specified node *)
val dump_cached_graph
  :  ?on_not_cached:[ `Ignore | `Raise ]
  -> ?time_nodes:bool
  -> ('i, 'o) Cell.t
  -> Dune_graph.Graph.t Fiber.t

module Lazy : sig
  type 'a t

  val of_val : 'a -> 'a t

  val create
    :  ?cutoff:('a -> 'a -> bool)
    -> ?name:string
    -> ?human_readable_description:(unit -> User_message.Style.t Pp.t)
    -> (unit -> 'a memo)
    -> 'a t

  val force : 'a t -> 'a memo
  val map : 'a t -> f:('a -> 'b) -> 'b t

  module Expert : sig
    (** Like [Lazy.create] but returns the underlying Memo [Cell], which can be
        useful for testing and debugging. *)
    val create
      :  ?cutoff:('a -> 'a -> bool)
      -> ?name:string
      -> ?human_readable_description:(unit -> User_message.Style.t Pp.t)
      -> (unit -> 'a memo)
      -> (unit, 'a) Cell.t * 'a t
  end
end

val lazy_
  :  ?cutoff:('a -> 'a -> bool)
  -> ?name:string
  -> ?human_readable_description:(unit -> User_message.Style.t Pp.t)
  -> (unit -> 'a t)
  -> 'a Lazy.t

module Implicit_output : sig
  type 'o t

  (** [produce] and [produce_opt] are used by effectful functions to produce
      output. *)
  val produce : 'o t -> 'o -> unit memo

  val produce_opt : 'o t -> 'o option -> unit memo

  (** [collect] and [forbid] take a potentially effectful function (one which
      may produce some implicit output) and turn it into a pure one (with
      explicit output if any). *)
  val collect : 'o t -> (unit -> 'a memo) -> ('a * 'o option) memo

  val forbid : (unit -> 'a memo) -> 'a memo

  module type Implicit_output = Implicit_output.Implicit_output

  (** Register a new type of implicit output. *)
  val add : (module Implicit_output with type t = 'o) -> 'o t
end

module With_implicit_output : sig
  type ('i, 'o) t

  val create
    :  string
    -> input:(module Input with type t = 'i)
    -> implicit_output:'io Implicit_output.t
    -> ('i -> 'o memo)
    -> ('i, 'o) t

  val exec : ('i, 'o) t -> 'i -> 'o memo
end

(** A memoization node that can be explicitly set and invalidated. *)
module Var : sig
  type 'a memo := 'a t
  type 'a t

  (** [create ~name] creates a new variable with the name [name].

      [name]s are currently only used for debugging and there is no validation, i.e., you
      can create multiple variables with the same name and Memo will not complain. *)
  val create : ?cutoff:('a -> 'a -> bool) -> 'a -> name:string -> 'a t

  (** [set t value] sets the value of [t] to [value]. Returns the corresponding
      invalidation that will trigger the subsequent re-evaluation of dependant nodes. *)
  val set : 'a t -> 'a -> Invalidation.t

  (** [read t] returns the value [t] and introduces a dependency on this node in the
      current Memo computation. *)
  val read : 'a t -> 'a memo
end

(** Memoization of polymorphic functions ['a input -> 'a output t]. The provided
    [id] function must be injective, i.e. there must be a one-to-one
    correspondence between [input]s and their [id]s. *)
module Poly (Function : sig
    type 'a input
    type 'a output

    val name : string
    val eval : 'a input -> 'a output t
    val to_dyn : _ input -> Dyn.t
    val id : 'a input -> 'a Type_eq.Id.t
  end) : sig
  val eval : 'a Function.input -> 'a Function.output t
end

(** Diagnostics features that affect performance but are useful for debugging. *)
module Debug : sig
  (** If [true], Memo will record the location of [Lazy.t] values. *)
  val track_locations_of_lazy_values : bool ref

  (** If [true], Memo will perform additional checks of internal invariants. *)
  val check_invariants : bool ref

  (** If [true], Memo will print out some diagnostics. It's convenient to set
      this flag temporarily while debugging a test. *)
  val verbose_diagnostics : bool ref
end

module Metrics = Metrics

module For_tests : sig
  (** After executing a memoized function with a given name and input, it is
      possible to query which dependencies that function used during execution
      by calling [get_deps] with the name and input used during execution.

      Returns [None] if the dependencies were not computed yet. *)
  val get_deps : ('i, _) Table.t -> 'i -> (string option * Dyn.t) list option

  (** Forget all memoized values, forcing them to be recomputed on the next
      build run. *)
  val clear_memoization_caches : unit -> unit
end

(** A check point. This fiber should be used to:

    - yield if there are external events that have priority over the current
      memo computation
    - raise if the current computation was cancelled so that Memo can avoid
      unnecessary work *)
val check_point : unit Fiber.t ref
