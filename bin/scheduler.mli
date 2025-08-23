open Import

val maybe_clear_screen : details_hum:string list -> Dune_config.t -> unit

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
