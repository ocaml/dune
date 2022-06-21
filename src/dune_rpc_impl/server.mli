module Config : sig
  type t =
    { handler : Dune_rpc_server.t
    ; pool : Fiber.Pool.t
    ; backlog : int
    ; root : string
    }
end

type t

val create : root:string -> t

val config : t -> Config.t

type pending_build_action =
  | Build of Dune_rules.Dep_conf.t list * Decl.Build_outcome.t Fiber.Ivar.t

val pending_build_action : t -> pending_build_action Fiber.t

(** Stop accepting new rpc connections. Fiber returns when all existing
    connections terminate *)
val stop : unit -> unit Fiber.t

val run : Config.t -> Dune_stats.t option -> unit Fiber.t
