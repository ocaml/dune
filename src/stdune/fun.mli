(* TODO Replace with: {include module type of Stdlib.Fun}

   once we drop support for 4.07 *)

(** [id] is the identity function. For any argument [x], [id x] is [x]. *)
external id : 'a -> 'a = "%identity"

(** [const c] is a function that always returns the value [c]. For any argument
    [x], [(const c) x] is [c]. *)
val const : 'a -> _ -> 'a

(** [flip f] reverses the argument order of the binary function [f]. For any
    arguments [x] and [y], [(flip f) x y] is [f y x]. *)
val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c

(** [negate p] is the negation of the predicate function [p]. For any argument
    [x], [(negate p) x] is [not (p x)]. *)
val negate : ('a -> bool) -> 'a -> bool

(** {1:exception Exception handling} *)

(** [protect ~finally work] invokes [work ()] and then [finally ()] before
    [work ()] returns with its value or an exception. In the latter case the
    exception is re-raised after [finally ()]. If [finally ()] raises an
    exception, then the exception {!Finally_raised} is raised instead.

    [protect] can be used to enforce local invariants whether [work ()] returns
    normally or raises an exception. However, it does not protect against
    unexpected exceptions raised inside [finally ()] such as
    {!Stdlib.Out_of_memory}, {!Stdlib.Stack_overflow}, or asynchronous
    exceptions raised by signal handlers (e.g. {!Sys.Break}).

    Note: It is a {e programming error} if other kinds of exceptions are raised
    by [finally], as any exception raised in [work ()] will be lost in the event
    of a {!Finally_raised} exception. Therefore, one should make sure to handle
    those inside the finally. *)
val protect : finally:(unit -> unit) -> (unit -> 'a) -> 'a

(** [Finally_raised exn] is raised by [protect ~finally work] when [finally]
    raises an exception [exn]. This exception denotes either an unexpected
    exception or a programming error. As a general rule, one should not catch a
    [Finally_raised] exception except as part of a catch-all handler. *)
exception Finally_raised of exn
