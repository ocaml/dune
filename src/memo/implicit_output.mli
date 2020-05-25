(** Uses dynamic scope to collect implicit output produced by functions. *)

type 'o t

(** [produce] and [produce_opt] are used by effectful functions to produce
    output. *)
val produce : 'o t -> 'o -> unit

val produce_opt : 'o t -> 'o option -> unit

(** [collect*] and [forbid*] take a potentially effectful function (one which
    may produce some implicit output) and turn it into a pure one (with explicit
    output if any) *)
val collect_async : 'o t -> (unit -> 'a Fiber.t) -> ('a * 'o option) Fiber.t

val collect_sync : 'o t -> (unit -> 'a) -> 'a * 'o option

val forbid_async : (unit -> 'a Fiber.t) -> 'a Fiber.t

val forbid_sync : (unit -> 'a) -> 'a

module type Implicit_output = sig
  type t

  val name : string

  val union : t -> t -> t
end

(** Register a new type of implicit output. *)
val add : (module Implicit_output with type t = 'o) -> 'o t
