open Import

type t

val create : root:string -> Dune_stats.t option -> t

val listening_address : t -> Dune_rpc.Where.t

val stats : t -> Dune_stats.t option

type pending_build_action =
  | Build of Dune_rules.Dep_conf.t list * Decl.Build_outcome.t Fiber.Ivar.t

val pending_build_action : t -> pending_build_action Fiber.t

(** Stop accepting new rpc connections. Fiber returns when all existing
    connections terminate *)
val stop : unit -> unit Fiber.t

val run : t -> unit Fiber.t
