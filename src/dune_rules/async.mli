(** Like [Scheduler.async_exn] but can be disabled in [dune_config] *)
val async : (unit -> 'a) -> 'a Fiber.t
