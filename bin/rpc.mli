open Import

val client_term :
  Common.t -> (Dune_rpc_impl.Run.t -> Dune_rpc.Where.t -> 'a Fiber.t) -> 'a

val group : unit Term.Group.t
