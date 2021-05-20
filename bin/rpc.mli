open Import

val wait_for_server : Common.t -> Dune_rpc.Where.t

val client_term : Common.t -> (Common.t -> 'a Fiber.t) -> 'a

val group : unit Term.Group.t
