open Import

val maybe_clear_screen
  :  terminal_persistence:Dune_config.Terminal_persistence.t
  -> details_hum:string list
  -> unit

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

val go_with_rpc_server_and_console_status_reporting
  :  common:Common.t
  -> config:Dune_config.t
  -> (unit -> 'a Fiber.t)
  -> 'a
