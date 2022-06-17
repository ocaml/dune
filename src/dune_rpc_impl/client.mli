open Import

include
  Dune_rpc.Client.S
    with type 'a fiber := 'a Fiber.t
     and type chan := Csexp_rpc.Session.t
