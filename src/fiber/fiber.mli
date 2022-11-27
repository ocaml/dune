[@@@alert
unstable "The API of this library is not stable and may change without notice."]

[@@@alert "-unstable"]

(** Concurrency library

    This module implements
    {{:https://en.wikipedia.org/wiki/Structured_concurrency} "structured
    concurrency"}. *)

open! Stdune

(** {1 Generals} *)

(** Type of fiber. A fiber represent a suspended computation. Note that using
    the same fiber twice will execute it twice, which is probably not what you
    want. To share the result of a fiber, use an [Ivar.t]. *)
type 'a t

type 'a fiber := 'a t

(** Create a fiber that has already terminated. *)
val return : 'a -> 'a t

(** Converts a thunk to a fiber, making sure the thunk runs in the context of
    the fiber (rather than applied in the current context).

    Equivalent to [(>>=) (return ())], but more explicit. *)
val of_thunk : (unit -> 'a t) -> 'a t

(** Fiber that never completes. *)
val never : 'a t

module O : sig
  (** [>>>] is a sequencing operator. [a >>> b] is the fiber that first executes
      [a] and then [b]. *)
  val ( >>> ) : unit t -> 'a t -> 'a t

  (** [>>=] is similar to [>>>] except that the result of the first fiber is
      used to create the second one. *)
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  (** [t >>| f] is the same as [t >>= fun x -> return (f x)] but slightly more
      efficient. *)
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  (** Similar to [fork_and_join] *)
  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
end

val map : 'a t -> f:('a -> 'b) -> 'b t

val bind : 'a t -> f:('a -> 'b t) -> 'b t

(** {1 Joining} *)

(** The following combinators are helpers to combine the result of several
    fibers into one. Note that they do not introduce parallelism. *)

val both : 'a t -> 'b t -> ('a * 'b) t

(** Execute a list of fibers in sequence. We use the short name to conform with
    the [Applicative] interface.*)
val all : 'a t list -> 'a list t

val sequential_map : 'a list -> f:('a -> 'b t) -> 'b list t

val sequential_iter : 'a list -> f:('a -> unit t) -> unit t

(** {1 Forking + joining} *)

(** The following functions combine forking 2 or more fibers followed by joining
    the results. The execution of the various fibers might be interleaved,
    however once the combining fiber has terminated, it is guaranteed that there
    are no fibers lingering around. *)

(** Start two fibers and wait for their result. Note that this function combines
    both successes and errors: if one of the computations fails, we let the
    other one run to completion, to give it a chance to raise its errors too.
    All other parallel execution combinators have the same error semantics. *)
val fork_and_join : (unit -> 'a t) -> (unit -> 'b t) -> ('a * 'b) t

(** Same but assume the first fiber returns [unit]. *)
val fork_and_join_unit : (unit -> unit t) -> (unit -> 'a t) -> 'a t

(** Map a list in parallel. *)
val parallel_map : 'a list -> f:('a -> 'b t) -> 'b list t

(* CR-someday amokhov: For discoverability and fast code completion it would be
   better to name functions [foo_sequentially] rather than [sequential_foo]. *)

(** Like [all] but executes the fibers concurrently. *)
val all_concurrently : 'a t list -> 'a list t

(** Like [all_concurrently] but is specialized for [unit] fibers. The advantage
    being that it doesn't allocate a return list. *)
val all_concurrently_unit : unit t list -> unit t

(** Iter over a list in parallel. *)
val parallel_iter : 'a list -> f:('a -> unit t) -> unit t

val parallel_iter_set :
     (module Set.S with type elt = 'a and type t = 's)
  -> 's
  -> f:('a -> unit t)
  -> unit t

val sequential_iter_seq : 'a Seq.t -> f:('a -> unit t) -> unit t

(** Provide efficient parallel iter/map functions for maps. *)
module Make_map_traversals (Map : Map.S) : sig
  val parallel_iter : 'a Map.t -> f:(Map.key -> 'a -> unit t) -> unit t

  val parallel_map : 'a Map.t -> f:(Map.key -> 'a -> 'b t) -> 'b Map.t t
end
[@@inline always]

(** {1 Local storage} *)

(** Variables local to a fiber *)
module Var : sig
  type 'a t

  (** Create a new variable *)
  val create : unit -> 'a t

  (** [get var] reads the value of [var]. *)
  val get : 'a t -> 'a option fiber

  (** Same as [get] but raises if [var] is unset. *)
  val get_exn : 'a t -> 'a fiber

  (** [set var value fiber] sets [var] to [value] during the execution of
      [fiber].

      For instance, the following fiber always evaluate to [true]:

      {[
        set v x (get_exn v >>| fun y -> x = y)
      ]} *)
  val set : 'a t -> 'a -> (unit -> 'b fiber) -> 'b fiber

  val unset : 'a t -> (unit -> 'b fiber) -> 'b fiber
end

(** {1 Error handling} *)

(** [with_error_handler f ~on_error] calls [on_error] for every exception raised
    during the execution of [f]. This include exceptions raised when calling
    [f ()] or during the execution of fibers after [f ()] has returned.
    Exceptions raised by [on_error] are passed on to the parent error handler.

    It is guaranteed that after the fiber has returned a value, [on_error] will
    never be called. *)
val with_error_handler :
  (unit -> 'a t) -> on_error:(Exn_with_backtrace.t -> Nothing.t t) -> 'a t

val map_reduce_errors :
     (module Monoid with type t = 'a)
  -> on_error:(Exn_with_backtrace.t -> 'a t)
  -> (unit -> 'b t)
  -> ('b, 'a) result t

(** [collect_errors f] is:
    [fold_errors f ~init:\[\] ~on_error:(fun e l -> e :: l)] *)
val collect_errors :
  (unit -> 'a t) -> ('a, Exn_with_backtrace.t list) Result.t t

(** [finalize f ~finally] runs [finally] after [f ()] has terminated, whether it
    fails or succeeds. *)
val finalize : (unit -> 'a t) -> finally:(unit -> unit t) -> 'a t

(** [reraise_all exns] re-raises all [exns] to the current error handler *)
val reraise_all : Exn_with_backtrace.t list -> 'a t

(** {1 Synchronization} *)

(** Write once variables *)
module Ivar : sig
  (** A ivar is a synchronization variable that can be written only once. *)
  type 'a t

  (** Create a new empty ivar. *)
  val create : unit -> 'a t

  (** Read the contents of the ivar. *)
  val read : 'a t -> 'a fiber

  (** Fill the ivar with the following value. This can only be called once for a
      given ivar. *)
  val fill : 'a t -> 'a -> unit fiber

  (** Return [Some x] is [fill t x] has been called previously. *)
  val peek : 'a t -> 'a option fiber
end

(** Mailbox variables *)
module Mvar : sig
  (** A mailbox variable can be thought of as a box that is either empty or
      full. [create ()] creates a new empty box, and [create_full x] creates a
      new full box containing [x].

      [read] removes the value from a full mailbox variable and returns it, but
      blocks if the mvar is currently empty. Symmetrically, [write] puts a value
      into the mvar but blocks if the mvar is already full. *)

  type 'a t

  val create : unit -> 'a t

  val create_full : 'a -> 'a t

  val read : 'a t -> 'a fiber

  val write : 'a t -> 'a -> unit fiber
end

(** State variables *)
module Svar : sig
  (** A "state variable" [Svar.t] differs from [Ivar.t] in that it can be
      updated multiple times. It is particularly useful if you'd like to observe
      the state of a process by subscribing to "interesting" (from your
      perspective) events.

      For example, a build system can have a complex state machine, switching
      between states like "waiting for source file changes", "building", etc.,
      by calling [Svar.write] on the corresponding state variable. If you don't
      care about most of the states but would like to observe all errors, you
      can use [Svar.wait ~until:is_error] to be woken up when an error happens.
      Other observers may have different preferences, expressed with different
      [until] predicates. You can also use the non-blocking [Svar.read] if you'd
      like to use polling instead. *)

  (** ['a t] is a state variable holding the value of type ['a] *)
  type 'a t

  (** [create init] creates a new state variable initialized with [init] *)
  val create : 'a -> 'a t

  (** [read t] returns the current value of [t] *)
  val read : 'a t -> 'a

  (** [wait t ~until] waits until [t]'s value satisfies the predicate [until].
      If the current value satisfies it, [wait] returns immediately, otherwise,
      the caller is blocked. *)
  val wait : 'a t -> until:('a -> bool) -> unit fiber

  (** [write t a] sets the current value of [t] to [a] *)
  val write : 'a t -> 'a -> unit fiber
end

module Mutex : sig
  type t

  val create : unit -> t

  val with_lock : t -> f:(unit -> 'a fiber) -> 'a fiber
end

module Throttle : sig
  (** Limit the number of jobs *)

  type t

  (** [create n] creates a throttler that allows to run [n] jobs at once *)
  val create : int -> t

  (** How many jobs can run at the same time *)
  val size : t -> int

  (** Change the number of jobs that can run at once *)
  val resize : t -> int -> unit fiber

  (** Execute a fiber, waiting if too many jobs are already running *)
  val run : t -> f:(unit -> 'a fiber) -> 'a fiber

  (** Return the number of jobs currently running *)
  val running : t -> int
end

val repeat_while : f:('a -> 'a option t) -> init:'a -> unit t

module Stream : sig
  (** Destructive streams that can be composed to pipelines.

      Streams can be finite or infinite. Streams have no storage and can only
      have one writer and/or one reader at any given time. If you'd like to
      access a stream concurrently, you need to protect it via a mutex.

      Trying to access the same side of a stream concurrently will result in an
      error. *)

  module In : sig
    (** Stream inputs.

        A [In.t] value represents the side of a stream where values can be read
        from. A stream input can only be consumed by one fiber at a time. Trying
        to access a stream input concurrently will result in an exception.

        When passing a stream input through one of the transformation functions
        such as {!filter_map} or iteration function, the original stream input
        can no longer be used for another purpose. *)
    type 'a t

    (** Create a stream that is fed from a generator function. Every time a
        value is requested, this function is called to produce a new value. If
        the function raises, the stream will no longer be usable. *)
    val create : (unit -> 'a option fiber) -> 'a t

    (** Create a stream that yields elements from the given list in order. *)
    val of_list : 'a list -> 'a t

    (** The empty stream. *)
    val empty : unit -> 'a t

    (** Consumes and returns the next element from the stream. Once the stream
        is exhausted, [read] always returns [None]. *)
    val read : 'a t -> 'a option fiber

    val filter_map : 'a t -> f:('a -> 'b option) -> 'b t

    val sequential_iter : 'a t -> f:('a -> unit fiber) -> unit fiber

    val parallel_iter : 'a t -> f:('a -> unit fiber) -> unit fiber

    val append : 'a t -> 'a t -> 'a t

    val cons : 'a -> 'a t -> 'a t
  end

  module Out : sig
    (** Stream outputs.

        A [Out.t] value represents the side of a stream where values can be
        pushed to. Only one value can be pushed at a time. Trying to push two
        values concurrently will result in an error. *)
    type 'a t

    (** Create a stream output. The callback is the consumer for values pushed
        to the stream. *)
    val create : ('a option -> unit fiber) -> 'a t

    val write : 'a t -> 'a option -> unit fiber

    val null : unit -> 'a t
  end

  (** [connect i o] reads from [i] and writes to [o]. Closes [o] when [i] is
      exhausted. Returned fiber terminates when [i] is exhausted *)
  val connect : 'a In.t -> 'a Out.t -> unit fiber

  (** [supply i o] like [connect i o] but does not close [o] once [i] is
      exhausted, allowing more values to be pused to [o]. Returned fiber
      terminates when [i] is exhausted*)
  val supply : 'a In.t -> 'a Out.t -> unit fiber

  (** [pipe ()] returns [(i, o)] where values pushed through [o] can be read
      through [i]. *)
  val pipe : unit -> 'a In.t * 'a Out.t
end

module Pool : sig
  (** Pool is used to submit asynchronous tasks without waiting for their
      completion. *)
  type t

  (** Create a new pool. *)
  val create : unit -> t

  (** [running pool] returns whether it's possible to submit tasks to [pool] *)
  val running : t -> bool fiber

  (** [task pool ~f] submit [f] to be done in [pool]. Errors raised [pool] will
      not be raised in the current fiber, but inside the [Pool.run] fiber.

      If [running pool] returns [false], this function will raise a
      [Code_error]. *)
  val task : t -> f:(unit -> unit fiber) -> unit fiber

  (** [stop pool] stops the pool from receiving new tasks. After this function
      is called, [task pool ~f] will fail to submit new tasks.

      Note that stopping the pool does not prevent already queued tasks from
      running. *)
  val stop : t -> unit fiber

  (** [run pool] Runs all tasks submitted to [pool] in parallel. Errors raised
      by such tasks must be caught here.*)
  val run : t -> unit fiber
end

(** {1 Running fibers} *)

type fill = Fill : 'a Ivar.t * 'a -> fill

(** [run t ~iter] runs a fiber until it terminates. [iter] is used to implement
    the scheduler, it should block waiting for an event and return at least one
    ivar to fill. *)
val run : 'a t -> iter:(unit -> fill Nonempty_list.t) -> 'a

(** Advanced fiber execution *)
module Scheduler : sig
  (** Represent a fiber that has stalled. *)
  type 'a stalled

  (** The outcome of a step of execution. *)
  type 'a step =
    | Done of 'a
    | Stalled of 'a stalled

  (** [start t] starts executing a fiber. This advance the execution of the
      fiber as possible, until no more progress can be made. *)
  val start : 'a fiber -> 'a step

  (** Advance a stalled fiber as much as possible by filling a list of ivars.
      Once must call [advance] on a given [stalled] value only once. *)
  val advance : 'a stalled -> fill Nonempty_list.t -> 'a step
end

(** {1 Fiber cancellation} *)
module Cancel : sig
  (** This module provides a way to cancel long running computations.
      Cancellation is fully explicit and fibers must explicitly check for it at
      strategic points. *)

  type t

  val create : unit -> t

  (** Activate a cancellation request.

      [fire] is idempotent, so calling [fire t] more than once has no effect. *)
  val fire : t -> unit fiber

  (** Version of [fire] that is suitable to call from the [iter] callback of
      [Fiber.run]. *)
  val fire' : t -> fill list

  (** Return whether the given cancellation has been fired. *)
  val fired : t -> bool

  type 'a outcome =
    | Cancelled of 'a
    | Not_cancelled

  (** [with_handler t ~on_cancel f] runs [f ()] with a cancellation handler. If
      [t] is fired during the execution of [f], then [on_cancel] is called.

      The aim of [on_cancel] is to somehow cut short the execution of [f]. A
      typical example is a function running an external command. [on_cancel]
      might send a [KILL] signal to the command to abort its execution.

      If [f ()] finished before [t] is fired, then [on_cancel] will never be
      invoked. *)
  val with_handler :
       t
    -> (unit -> 'a fiber)
    -> on_cancel:(unit -> 'b fiber)
    -> ('a * 'b outcome) fiber
end

module Expert : sig
  (** This module offers no safety protections. It is only needed for maximizing
      performance in certain situations *)

  type 'a k

  (** [suspend k] call with the current continuation *)
  val suspend : ('a k -> unit) -> 'a t

  (** [resume k a] resume a suspended fiber with [a]. It is forbidden to call
      [resume] more than once on a a fiber. *)
  val resume : 'a k -> 'a -> unit t
end
