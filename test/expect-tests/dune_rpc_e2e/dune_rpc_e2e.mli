open Stdune

val with_dune_watch :
  ?env:string list -> string list -> (Pid.t -> 'a Fiber.t) -> 'a Fiber.t

val dune_wait : Dune_rpc_impl.Client.t -> unit Fiber.t

val run_client :
     ?handler:Dune_rpc_impl.Client.Handler.t
  -> (Dune_rpc_impl.Client.t -> 'a Fiber.t)
  -> 'a Fiber.t

val run : (unit -> 'a Fiber.t) -> 'a
