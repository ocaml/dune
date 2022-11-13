type server =
  { run : unit Fiber.t
  ; stop : unit Fiber.t
  ; ready : unit Fiber.t
  }

val with_background_rpc : server -> (unit -> 'a Fiber.t) -> 'a Fiber.t

val ensure_ready : unit -> unit Fiber.t

val stop : unit -> unit Fiber.t
