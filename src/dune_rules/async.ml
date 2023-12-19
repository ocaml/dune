open Import

let async f =
  match Config.get background_dune_rules with
  | `Enabled -> Dune_engine.Scheduler.async_exn f
  | `Disabled -> Fiber.return (f ())
;;
