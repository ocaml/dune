open Stdune

val with_dune_watch
  :  ?watch_mode_args:string list
  -> ?env:string list
  -> (Pid.t -> 'a Fiber.t)
  -> 'a Fiber.t

val dune_build : Rpc.Client.t -> string -> unit Fiber.t

val run_client
  :  ?handler:Rpc.Client.Handler.t
  -> (Rpc.Client.t -> 'a Fiber.t)
  -> 'a Fiber.t

val run : (unit -> 'a Fiber.t) -> 'a
