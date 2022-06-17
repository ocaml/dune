(** Rpc related functions *)

module Config : sig
  type t =
    | Client
    | Server of
        { handler : Dune_rpc_server.t
        ; pool : Fiber.Pool.t
        ; backlog : int
        ; root : string
        }
end

(** Stop accepting new rpc connections. Fiber returns when all existing
    connections terminate *)
val stop : unit -> unit Fiber.t

val run : Config.t -> Dune_stats.t option -> unit Fiber.t
