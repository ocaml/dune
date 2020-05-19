(** Concurrency library *)

open! Stdune

(** {1 Generals} *)

(** Type of fiber. A fiber represent a suspended computation. Note that using
    the same fiber twice will execute it twice, which is probably not what you
    want. To share the result of a fiber, use an [Ivar.t]. *)
type 'a t

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
end

val map : 'a t -> f:('a -> 'b) -> 'b t

val bind : 'a t -> f:('a -> 'b t) -> 'b t

(** {1 Forking execution} *)

module Future : sig
  type 'a fiber

  (** A future represent a promise that will eventually yield a value. It is
      used to represent the result of a fiber running in the background. *)
  type 'a t

  (** Wait for the given future to yield a value. *)
  val wait : 'a t -> 'a fiber

  (** Return [Some x] if [t] has already returned. *)
  val peek : 'a t -> 'a option fiber
end
with type 'a fiber := 'a t

(** [fork f] creates a sub-fiber and return a [Future.t] to wait its result. *)
val fork : (unit -> 'a t) -> 'a Future.t t

(** [nfork l] is similar to [fork] but creates [n] sub-fibers. *)
val nfork : (unit -> 'a t) list -> 'a Future.t list t

(** [nfork_map l ~f] is the same as [nfork (List.map l ~f:(fun x () -> f x))]
    but more efficient. *)
val nfork_map : 'a list -> f:('a -> 'b t) -> 'b Future.t list t

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

(** For two fibers and wait for their results:

    {[
      let fork_and_join f g =
        fork f >>= fun a ->
        fork g >>= fun b -> both (Future.wait a) (Future.wait b)
    ]} *)
val fork_and_join : (unit -> 'a t) -> (unit -> 'b t) -> ('a * 'b) t

(** Same but assume the first fiber returns [unit]:

    {[
      let fork_and_join_unit f g =
        fork f >>= fun a ->
        fork g >>= fun b -> Future.wait a >>> Future.wait b
    ]} *)
val fork_and_join_unit : (unit -> unit t) -> (unit -> 'a t) -> 'a t

(** Map a list in parallel:

    {[
      let parallel_map l ~f =
        nfork_map l ~f >>= fun futures -> all (List.map futures ~f:Future.wait)
    ]} *)
val parallel_map : 'a list -> f:('a -> 'b t) -> 'b list t

(** Iter over a list in parallel:

    {[
      let parallel_iter l ~f =
        nfork_map l ~f >>= fun futures ->
        all_unit (List.map futures ~f:Future.wait)
    ]} *)
val parallel_iter : 'a list -> f:('a -> unit t) -> unit t

(** {1 Local storage} *)

(** Variables local to a fiber *)
module Var : sig
  type 'a fiber = 'a t

  type 'a t

  (** Create a new variable *)
  val create : unit -> 'a t

  (** [get var] reads the value of [var]. *)
  val get : 'a t -> 'a option

  (** Same as [get] but raises if [var] is unset. *)
  val get_exn : 'a t -> 'a

  (** [set var value fiber] sets [var] to [value] during the execution of
      [fiber].

      For instance, the following fiber always evaluate to [true]:

      {[ set v x (get_exn v >>| fun y -> x = y) ]} *)
  val set : 'a t -> 'a -> (unit -> 'b fiber) -> 'b fiber

  val set_sync : 'a t -> 'a -> (unit -> 'b) -> 'b

  val unset : 'a t -> (unit -> 'b fiber) -> 'b fiber

  val unset_sync : 'a t -> (unit -> 'b) -> 'b
end
with type 'a fiber := 'a t

(** {1 Error handling} *)

(** [with_error_handler f ~on_error] calls [on_error] for every exception raised
    during the execution of [f]. This include exceptions raised when calling
    [f ()] or during the execution of fibers after [f ()] has returned.
    Exceptions raised by [on_error] are passed on to the parent error handler.

    It is guaranteed that after the fiber has returned a value, [on_error] will
    never be called. *)
val with_error_handler :
  (unit -> 'a t) -> on_error:(Exn_with_backtrace.t -> unit) -> 'a t

(** [fold_errors f ~init ~on_error] calls [on_error] for every exception raised
    during the execution of [f]. This include exceptions raised when calling
    [f ()] or during the execution of fibers after [f ()] has returned.

    Exceptions raised by [on_error] are passed on to the parent error handler. *)
val fold_errors :
     (unit -> 'a t)
  -> init:'b
  -> on_error:(Exn_with_backtrace.t -> 'b -> 'b)
  -> ('a, 'b) Result.t t

(** [collect_errors f] is:
    [fold_errors f ~init:\[\] ~on_error:(fun e l -> e :: l)] *)
val collect_errors :
  (unit -> 'a t) -> ('a, Exn_with_backtrace.t list) Result.t t

(** [finalize f ~finally] runs [finally] after [f ()] has terminated, whether it
    fails or succeeds. *)
val finalize : (unit -> 'a t) -> finally:(unit -> unit t) -> 'a t

(** {1 Synchronization} *)

(** Write once variables *)
module Ivar : sig
  type 'a fiber

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
with type 'a fiber := 'a t

module Mutex : sig
  type 'a fiber = 'a t

  type t

  val create : unit -> t

  val with_lock : t -> (unit -> 'a fiber) -> 'a fiber
end
with type 'a fiber := 'a t

(** {1 Running fibers} *)

(** [run t] runs a fiber. If the fiber doesn't complete immediately, [run t]
    returns [None]. *)
val run : 'a t -> 'a option
