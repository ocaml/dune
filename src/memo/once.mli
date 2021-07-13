open Stdune

(* CR-someday amokhov: We could try to encapsulate all Memo's cycle-detection
   logic in [Once]. Creating DAG nodes and edges should be relatively simple,
   but passing information about Memo's call stack seems a bit awkward. *)

(** A fiber that can be shared but will be computed at most once. An equivalent
    of ['a Lazy.t] but for asynchronous computations. *)
type 'a t

(** Note that side effects of the shared fiber will happen only when the fiber
    returned by [force] is executed. *)
val create : must_not_raise:(unit -> 'a Fiber.t) -> 'a t

(** Execute a shared fiber, or block until the result becomes available if the
    fiber is currently being executed. Note that this can introduce deadlocks if
    fibers depend on each other in a cycle. [on_blocking] will be called before
    blocking the caller, giving it a chance to detect a cycle and return [Error]
    to bail out early. *)
val force :
     'a t
  -> on_blocking:(unit -> (unit, 'b) result Fiber.t)
  -> ('a, 'b) result Fiber.t
