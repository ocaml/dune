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

(** {1 Joining} *)

(** The following combinators are helpers to combine the result of several
    fibers into one. Note that they do not introduce parallelism. *)

val both : 'a t -> 'b t -> ('a * 'b) t

val sequential_map : 'a list -> f:('a -> 'b t) -> 'b list t

val sequential_iter : 'a list -> f:('a -> unit t) -> unit t

(** {1 Forking + joining} *)

(** The following functions combine forking 2 or more fibers followed by joining
    the results. The execution of the various fibers might be interleaved,
    however once the combining fiber has terminated, it is guaranteed that there
    are no fibers lingering around. *)

(** Start two fibers and wait for their results. *)
val fork_and_join : (unit -> 'a t) -> (unit -> 'b t) -> ('a * 'b) t

(** Same but assume the first fiber returns [unit]. *)
val fork_and_join_unit : (unit -> unit t) -> (unit -> 'a t) -> 'a t

(** Map a list in parallel. *)
val parallel_map : 'a list -> f:('a -> 'b t) -> 'b list t

(** Iter over a list in parallel. *)
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

(** Mailbox variables *)
module Mvar : sig
  type 'a fiber

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
with type 'a fiber := 'a t

module Mutex : sig
  type 'a fiber = 'a t

  type t

  val create : unit -> t

  val with_lock : t -> (unit -> 'a fiber) -> 'a fiber
end
with type 'a fiber := 'a t

module Throttle : sig
  (** Limit the number of jobs *)

  type 'a fiber = 'a t

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
with type 'a fiber := 'a t

module Sequence : sig
  type 'a fiber = 'a t

  type 'a t = 'a node fiber

  and 'a node =
    | Nil
    | Cons of 'a * 'a t

  val sequential_iter : 'a t -> f:('a -> unit fiber) -> unit fiber

  (** [parallel_iter t ~f] is the same as:

      {[
        let rec loop t ~f =
          t >>= function
          | Nil -> return ()
          | Cons (x, t) ->
            fork_and_join_unit (fun () -> f x) (fun () -> loop t ~f)
      ]}

      except that if the sequence is infinite, the above code would leak memory
      while [parallel_iter] does not. This function can typically be used to
      process a sequence of events. *)
  val parallel_iter : 'a t -> f:('a -> unit fiber) -> unit fiber
end
with type 'a fiber := 'a t

(** {1 Running fibers} *)

type fill = Fill : 'a Ivar.t * 'a -> fill

(** [run t ~iter] runs a fiber until it terminates. [iter] is used to implement
    the scheduler, it should block waiting for an event and return an ivar to
    fill. *)
val run : 'a t -> iter:(unit -> fill) -> 'a
