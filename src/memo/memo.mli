open !Stdune

type ('input, 'output, 'f) t

val on_already_reported :
  (Exn_with_backtrace.t -> Nothing.t) -> unit

module Sync : sig
  type nonrec ('i, 'o) t = ('i, 'o, 'i -> 'o) t
end

module Async : sig
  type nonrec ('i, 'o) t = ('i, 'o, 'i -> 'o Fiber.t) t
end

(** A stack frame within a computation. *)
module Stack_frame : sig

  type ('input, 'output, 'f) memo = ('input, 'output, 'f) t

  type t

  val pp : Format.formatter -> t -> unit
  val to_dyn : t -> Dyn.t

  val equal : t -> t -> bool
  val compare : t -> t -> Ordering.t

  val name : t -> string
  val input : t -> Dyn.t

  (** Checks if the stack frame is a frame of the given memoized function
      and if so, returns [Some i] where [i] is the argument of the function. *)
  val as_instance_of : t -> of_:('input, _, _) memo -> 'input option
end

module Cycle_error : sig
  type t

  exception E of t

  (** Get the list of stack frames in this cycle. *)
  val get : t -> Stack_frame.t list

  (** Return the stack leading to the node which raised the cycle. *)
  val stack : t -> Stack_frame.t list
end

(** Restart the system. Cached values with a [Current_run] lifetime
    are forgotten, pending computations are cancelled. *)
val reset : unit -> unit

module Function_type : sig
  type ('a, 'b, 'f) t =
    | Sync : ('a, 'b, ('a -> 'b)) t
    | Async : ('a, 'b, ('a -> 'b Fiber.t)) t
end

module type Output_simple = sig
  type t
  val to_dyn : t -> Dyn.t
end

module type Output_allow_cutoff = sig
  type t
  val to_dyn : t -> Dyn.t
  val equal : t -> t -> bool
end

(**
   When we recompute the function and find that its output is the same as what we
   computed before, we can sometimes skip recomputing the values that depend on it.

   [Allow_cutoff] specifies how to compare the output values for that purpose.

   Note that currently Dune wipes all memoization caches on every run, so
   cutoff is not effective.
*)
module Output : sig
  type 'o t =
    | Simple of (module Output_simple with type t = 'o)
    | Allow_cutoff of (module Output_allow_cutoff with type t = 'o)
end

module type Input = sig
  type t
  val to_dyn : t -> Dyn.t
  include Table.Key with type t := t
end

module Visibility : sig
  type 'i t =
    | Hidden
    | Public of 'i Dune_lang.Decoder.t
end

(** [create name ~doc ~input ~visibility ~output f_type f] creates a memoized
    version of [f]. The result of [f] for a given input is cached, so that the
    second time [exec t x] is called, the previous result is re-used if
    possible.

    [exec t x] tracks what calls to other memoized function [f x] performs. When
    the result of such dependent call changes, [exec t x] will automatically
    recompute [f x].

    Running the computation may raise [Memo.Cycle_error.E] if a cycle is
    detected.

    Both simple functions (synchronous) and functions returning fibers
    (asynchronous ones) can be memoized, and the flavor is selected by [f_type].

    [visibility] determines whether the function is user-facing or internal and
    if it's user-facing then how to parse the values written by the user.
*)
val create
  :  string
  -> doc:string
  -> input:(module Input with type t = 'i)
  -> visibility:'i Visibility.t
  -> output:('o Output.t)
  -> ('i, 'o, 'f) Function_type.t
  -> 'f
  -> ('i, 'o, 'f) t

val create_hidden
  :  string
  -> doc:string
  -> input:(module Input with type t = 'i)
  -> ('i, 'o, 'f) Function_type.t
  -> 'f
  -> ('i, 'o, 'f) t

(** Check whether we already have a value for the given call *)
val peek : ('i, 'o, _) t -> 'i -> 'o option
val peek_exn : ('i, 'o, _) t -> 'i -> 'o

(** Execute a memoized function *)
val exec : (_, _, 'f) t -> 'f

(** After running a memoization function with a given name and
    input, it is possible to query which dependencies that function
    used during execution by calling [get_deps] with the name and
    input used during execution.

    Returns [None] if the dependencies were not computed yet.
*)
val get_deps : ('i, _, _) t -> 'i -> (string * Dyn.t) list option

(** Print the memoized call stack during execution. This is useful for
    debugging purposes. *)
val dump_stack : unit -> unit

val pp_stack : Format.formatter -> unit -> unit

(** Get the memoized call stack during the execution of a memoized function. *)
val get_call_stack : unit -> Stack_frame.t list

(** Call a memoized function by name *)
val call : string -> Dune_lang.Ast.t -> Dyn.t Fiber.t

module Function_info : sig
  type t =
    { name : string
    ; doc  : string
    }
end

(** Return the list of registered functions *)
val registered_functions : unit -> Function_info.t list

(** Lookup function's info *)
val function_info : string -> Function_info.t

module Lazy : sig
  type +'a t

  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  val bind : 'a t -> f:('a -> 'b t) -> 'b t

  val create : (unit -> 'a) -> 'a t
  val of_val : 'a -> 'a t
  val force : 'a t -> 'a
end

val lazy_ : (unit -> 'a) -> 'a Lazy.t

module With_implicit_output : sig

  type ('i, 'o, 'f) t

  val create
    :  string
    -> doc:string
    -> input:(module Input with type t = 'i)
    -> visibility:'i Visibility.t
    -> output:(module Output_simple with type t = 'o)
    -> implicit_output:('io Implicit_output.t)
    -> ('i, 'o, 'f) Function_type.t
    -> 'f
    -> ('i, 'o, 'f) t

  val exec : (_, _, 'f) t -> 'f
end

module Implicit_output = Implicit_output
