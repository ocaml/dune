open Stdune

val with_dune_watch : ?env:string list -> (Pid.t -> 'a Fiber.t) -> 'a Fiber.t

val dune_build : Dune_rpc_client.Client.t -> string -> unit Fiber.t

val run_client :
     ?handler:Dune_rpc_client.Client.Handler.t
  -> (Dune_rpc_client.Client.t -> 'a Fiber.t)
  -> 'a Fiber.t

val run : (unit -> 'a Fiber.t) -> 'a
