open! Import
open Node

(** Wrap an exception in [Non_reproducible] to tell Memo not to cache it. We catch it,
    unwrap, and re-raise without the wrapper. *)
exception Non_reproducible of exn

(** A hook run at the start of each computation, used for cooperative cancellation. *)
val check_point : unit Fiber.t ref

(** The run in which a non-reproducible error was last computed. [reset_if_necessary] reads
    it to force a new run even on an empty invalidation. *)
val last_saw_non_reproducible_exn_at : Run.t ref

(** Run [f], reporting its errors to the active handler and collecting them. *)
val report_and_collect_errors
  :  (unit -> 'a Fiber.t)
  -> ('a, Collect_errors_monoid.t) result Fiber.t

(** Restore-or-compute a memoized node, returning its value (reraising cached errors). *)
val exec_dep_node : ('i, 'o) Dep_node.t -> 'o Fiber.t
