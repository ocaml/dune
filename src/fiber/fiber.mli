(** Concurrency library *)

open! Stdune

(** {1 Generals} *)

(** Type of fiber. A fiber represent a suspended computation. Note that using
    the same fiber twice will execute it twice, which is probably not what you
    want. To share the result of a fiber, use an [Ivar.t]. *)
type 'a t

val return : 'a -> 'a t
(** Create a fiber that has already terminated. *)

val of_thunk : (unit -> 'a t) -> 'a t
(** Converts a thunk to a fiber, making sure the thunk runs in the context of
    the fiber (rather than applied in the current context).

    Equivalent to [(>>=) (return ())], but more explicit. *)

val never : 'a t
(** Fiber that never completes. *)

module O : sig
  val ( >>> ) : unit t -> 'a t -> 'a t
  (** [>>>] is a sequencing operator. [a >>> b] is the fiber that first executes
      [a] and then [b]. *)

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  (** [>>=] is similar to [>>>] except that the result of the first fiber is
      used to create the second one. *)

  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  (** [t >>| f] is the same as [t >>= fun x -> return (f x)] but slightly more
      efficient. *)

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
end

val map : 'a t -> f:('a -> 'b) -> 'b t

val bind : 'a t -> f:('a -> 'b t) -> 'b t

(** {1 Forking execution} *)

module Future : sig
  type 'a fiber

  (** A future represent a promise that will eventually yield a value. It is
      used to represent the result of a fiber running in the background. *)
  type 'a t

  val wait : 'a t -> 'a fiber
  (** Wait for the given future to yield a value. *)

  val peek : 'a t -> 'a option
  (** Return [Some x] if [t] has already returned. *)
end
with type 'a fiber := 'a t

val fork : (unit -> 'a t) -> 'a Future.t t
(** [fork f] creates a sub-fiber and return a [Future.t] to wait its result. *)

val nfork : (unit -> 'a t) list -> 'a Future.t list t
(** [nfork l] is similar to [fork] but creates [n] sub-fibers. *)

val nfork_map : 'a list -> f:('a -> 'b t) -> 'b Future.t list t
(** [nfork_map l ~f] is the same as [nfork (List.map l ~f:(fun x () -> f x))]
    but more efficient. *)

(** {1 Joining} *)

(** The following combinators are helpers to combine the result of several
    fibers into one. Note that they do not introduce parallelism. *)

val both : 'a t -> 'b t -> ('a * 'b) t

val sequential_map : 'a list -> f:('a -> 'b t) -> 'b list t

val sequential_iter : 'a list -> f:('a -> unit t) -> unit t

(** {1 Forking + joining} *)

(** The following functions combine forking 2 or more fibers followed by joining
    the results. For every function, we give an equivalent implementation using
    the more basic functions as documentation. Note however that these functions
    are implemented as primitives and so are more efficient that the suggested
    implementation. *)

val fork_and_join : (unit -> 'a t) -> (unit -> 'b t) -> ('a * 'b) t
(** For two fibers and wait for their results:

    {[
      let fork_and_join f g =
        fork f >>= fun a ->
        fork g >>= fun b -> both (Future.wait a) (Future.wait b)
    ]} *)

val fork_and_join_unit : (unit -> unit t) -> (unit -> 'a t) -> 'a t
(** Same but assume the first fiber returns [unit]:

    {[
      let fork_and_join_unit f g =
        fork f >>= fun a ->
        fork g >>= fun b -> Future.wait a >>> Future.wait b
    ]} *)

val parallel_map : 'a list -> f:('a -> 'b t) -> 'b list t
(** Map a list in parallel:

    {[
      let parallel_map l ~f =
        nfork_map l ~f >>= fun futures -> all (List.map futures ~f:Future.wait)
    ]} *)

val parallel_iter : 'a list -> f:('a -> unit t) -> unit t
(** Iter over a list in parallel:

    {[
      let parallel_iter l ~f =
        nfork_map l ~f >>= fun futures ->
        all_unit (List.map futures ~f:Future.wait)
    ]} *)

(** {1 Execute once fibers} *)

module Once : sig
  type 'a fiber = 'a t

  type 'a t

  val create : (unit -> 'a fiber) -> 'a t

  val get : 'a t -> 'a fiber
  (** [get t] returns the value of [t]. If [get] was never called before on this
      [t], it is executed at this point, otherwise returns a fiber that waits
      for the fiber from the first call to [get t] to terminate. *)

  val peek : 'a t -> 'a option
  (** [peek t] returns [Some v] if [get t] has already been called and has
      yielded a value [v]. *)

  val peek_exn : 'a t -> 'a
end
with type 'a fiber := 'a t

(** {1 Local storage} *)

(** Variables local to a fiber *)
module Var : sig
  type 'a fiber = 'a t

  type 'a t

  val create : unit -> 'a t
  (** Create a new variable *)

  val get : 'a t -> 'a option
  (** [get var] reads the value of [var]. *)

  val get_exn : 'a t -> 'a
  (** Same as [get] but raises if [var] is unset. *)

  val set : 'a t -> 'a -> (unit -> 'b fiber) -> 'b fiber
  (** [set var value fiber] sets [var] to [value] during the execution of
      [fiber].

      For instance, the following fiber always evaluate to [true]:

      {[ set v x (get_exn v >>| fun y -> x = y) ]} *)

  val set_sync : 'a t -> 'a -> (unit -> 'b) -> 'b

  val unset : 'a t -> (unit -> 'b fiber) -> 'b fiber

  val unset_sync : 'a t -> (unit -> 'b) -> 'b
end
with type 'a fiber := 'a t

(** {1 Error handling} *)

val with_error_handler :
  (unit -> 'a t) -> on_error:(Exn_with_backtrace.t -> unit) -> 'a t
(** [with_error_handler f ~on_error] calls [on_error] for every exception raised
    during the execution of [f]. This include exceptions raised when calling
    [f ()] or during the execution of fibers after [f ()] has returned.
    Exceptions raised by [on_error] are passed on to the parent error handler.

    It is guaranteed that after the fiber has returned a value, [on_error] will
    never be called. *)

val fold_errors :
     (unit -> 'a t)
  -> init:'b
  -> on_error:(Exn_with_backtrace.t -> 'b -> 'b)
  -> ('a, 'b) Result.t t
(** [fold_errors f ~init ~on_error] calls [on_error] for every exception raised
    during the execution of [f]. This include exceptions raised when calling
    [f ()] or during the execution of fibers after [f ()] has returned.

    Exceptions raised by [on_error] are passed on to the parent error handler. *)

val collect_errors :
  (unit -> 'a t) -> ('a, Exn_with_backtrace.t list) Result.t t
(** [collect_errors f] is:
    [fold_errors f ~init:\[\] ~on_error:(fun e l -> e :: l)] *)

val finalize : (unit -> 'a t) -> finally:(unit -> unit t) -> 'a t
(** [finalize f ~finally] runs [finally] after [f ()] has terminated, whether it
    fails or succeeds. *)

(** {1 Synchronization} *)

(** Write once variables *)
module Ivar : sig
  type 'a fiber = 'a t

  (** A ivar is a synchronization variable that can be written only once. *)
  type 'a t

  val create : unit -> 'a t
  (** Create a new empty ivar. *)

  val read : 'a t -> 'a fiber
  (** Read the contents of the ivar. *)

  val fill : 'a t -> 'a -> unit fiber
  (** Fill the ivar with the following value. This can only be called once for a
      given ivar. *)

  val peek : 'a t -> 'a option
  (** Return [Some x] is [fill t x] has been called previously. *)
end
with type 'a fiber := 'a t

module Mutex : sig
  type 'a fiber = 'a t

  type t

  val create : unit -> t

  val with_lock : t -> (unit -> 'a fiber) -> 'a fiber
end
with type 'a fiber := 'a t

(** {1 Running fibers} *)

val yield : unit -> unit t
(** Wait for one iteration of the scheduler *)

val run : 'a t -> 'a
(** [run t] runs a fiber until it (and all the fibers it forked) terminate.
    Returns the result if it's determined in the end, otherwise raises [Never]. *)

exception Never
