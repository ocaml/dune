open! Import
open Node

(** Collects the dependencies discovered by a running computation. Kept in fiber-local
    storage so that parallel branches can each collect into their own sub-collector. *)
type t

val create : unit -> t

(** Run [f x] with [t] as the active collector. *)
val run_apply : t -> f:('b -> 'a Fiber.t) -> 'b -> 'a Fiber.t

(** The dependencies collected so far, as a series-parallel graph. *)
val get : t -> Dep_node.packed Deps.t

(** Record a dependency on [dep_node] in the active collector, if there is one. *)
val add_dep_from_caller : ('i, 'o) Dep_node.t -> unit Fiber.t

(** A way to run one parallel thread while collecting its dependencies separately. *)
type run_thread = { run : 'a 'b. f:('b -> 'a Fiber.t) -> 'b -> 'a Fiber.t } [@@unboxed]

(** Run [num_threads] parallel threads, recording their dependencies as a single parallel
    section in the active collector. [k] is called with [None] when [num_threads < 2] or
    when there is no active collector, in which case dependencies are collected
    sequentially. *)
val run_parallel : num_threads:int -> (run_thread option -> 'a Fiber.t) -> 'a Fiber.t
