open Import

val no_build_no_rpc : config:Dune_config.t -> (unit -> 'a Fiber.t) -> 'a

val go_without_rpc_server
  :  common:Common.t
  -> config:Dune_config.t
  -> (unit -> 'a Fiber.t)
  -> 'a

val go_with_rpc_server
  :  common:Common.t
  -> config:Dune_config.t
  -> (unit -> 'a Fiber.t)
  -> 'a

val go_with_rpc_server_and_file_watcher
  :  common:Common.t
  -> config:Dune_config.t
  -> rpc_server:Dune_rpc_impl.Server.t
  -> (unit -> 'a Fiber.t)
  -> 'a
