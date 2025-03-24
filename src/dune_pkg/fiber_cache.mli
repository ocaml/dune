open! Import

(** Associates asynchronously-computed values of type ['v] with keys of type ['k] *)
type ('k, 'v) t

val create : (module Table.Key with type t = 'k) -> ('k, 'v) t

(** Retrieve a value from the cache, computing it with [f] if it doesn't exist.
    If [f] raises then this function will also raise the same exceptions and
    future calls to this function will also raise the same exceptions rather
    than attempting to recompute the value. *)
val find_or_add : ('k, 'v) t -> 'k -> f:(unit -> 'v Fiber.t) -> 'v Fiber.t

(** [to_table t] returns a table with the same associations as [t]. *)
val to_table : ('k, 'v) t -> ('k, 'v) Table.t Fiber.t
