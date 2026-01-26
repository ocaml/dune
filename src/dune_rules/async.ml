open Import
open Dune_scheduler

let async f =
  match Config.get background_dune_rules with
  | `Enabled -> Scheduler.async_exn f
  | `Disabled -> Fiber.return (f ())
;;
