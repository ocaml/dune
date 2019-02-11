open !Stdune

(** A stack frame within a computation. *)
module Stack_frame : sig
  type t

  val pp : Format.formatter -> t -> unit

  val equal : t -> t -> bool
  val compare : t -> t -> Ordering.t

  val name : t -> string
  val input : t -> Sexp.t
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

module type S = Memo_intf.S with type stack_frame := Stack_frame.t
module type Input = Memo_intf.Input
module type Output = Memo_intf.Output
module type Decoder = Memo_intf.Decoder

module Make(Input : Input)(Decoder : Decoder with type t := Input.t)
  : S with type input := Input.t

(** Same as [Make] except that registered functions won't be available
    through [dune compute]. In particular, it is not necessary to
    provide a decoder for the input. *)
module Make_hidden(Input : Input)
  : S with type input := Input.t

(** Print the memoized call stack during execution. This is useful for
    debugging purposes. *)
val dump_stack : unit -> unit

val pp_stack : Format.formatter -> unit -> unit

(** Get the memoized call stack during the execution of a memoized function. *)
val get_call_stack : unit -> Stack_frame.t list

(** Call a memoized function by name *)
val call : string -> Dune_lang.Ast.t -> Sexp.t Fiber.t

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
